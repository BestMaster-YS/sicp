本文章主要内容是用 Typescript 实现 SICP 书中3.5章节。

## Lazy

在JavaScript中可以用Generator函数配合 yield 关键字进行惰性求值。

如下代码：

```typescript
type Lazy<T> = Generator<T>;

function lazy<T>(x: T) {
    return (function *() {
      	yield x;  
    })();
}

function force<T>(x: Lazy<T>): T {
    const forced = x.next();
    return forced.value as T;
}
```

我们制定一个规范，Lazy类型的值（被lazy包裹的）必须通过 force 函数去取。

当然仅仅是一个Lazy类型不能发挥什么作用，否则你完全可以通过一个函数去包装它，同样可以达到相同的目的，而且看起来有点蠢。

```typescript
const lazy = (x) => () => x;
const force = (lazy) => lazy();
```

但若是让流与惰性求值结合在一起，就比较有意思。

## Stream

先定义 Stream 的类型

```typescript
type EmptyStream = [];
type Stream<T = any> = EmptyStream | [T, Lazy<Stream<T>>];
```

Stream 流可能为空，或者为一个元组，元组的第一个值为正常值，第二个值为惰性的流。这样定义流的类型说明流可能是无限长的。Stream 的数据结构也可以是链表。

定义对流的基本操作

```typescript
const cons = <T>(head: T, tail: Stream<T>): Stream<T> => [head, lazy(tail)];

const isEmptyStream = (stream: Stream): stream is EmptyStream => stream.length === 0;

const car  = <T>(stream: Stream<T>): T | undefined => stream[0];
const cdr  = <T>(stream: Stream<T>): Stream<T> => isEmptyStream(stream) ? stream : force(stream[1]);
```

## Infinity Stream

现在我们其实已经可以去生成和操作流，但是我们先跳过这步，去实现无限长度的流。

例子：

```typescript
const infinityStream = <T>(v: T, operator = (v: T) => v): Stream<T> => cons(v, infinityStream(operator(v), operator));

const ones = infinityStream(1);
```

运行上面的代码会报错，因为 调用 cons 函数时，执行的代码为

head = v;

tail = infinityStream(operator(v), operator);

这样赋值时会对两个参数进行求值，就导致了死循环，所以我们需要改善下 cons，以及 lazy函数

```typescript
function lazy<T>(x: () => T) {
    return (function *() {
      	yield x();
    })();
}

function cons<T>(h: T, tail: () => Stream<T>): Stream<T> {
    return [h, lazy(tail)];
}

const infinityStream = <T>(v: T, operator = (v: T) => v): Stream<T> => cons(v, infinityStream(operator(v), operator));

const ones = infinityStream(1);
```

lazy 函数的参数改为传入一个函数，cons函数的第二个参数也改为一个函数。 无论是 cons 或者 lazy 对参数进行赋值时就不会死循环了。我们就定义了一个无限长度且所有元素为1的流。

接下我们定义一些常规操作

```typescript
const take = <T>(stream: Stream<T>, total: number): T[] => {
    if (total <= 0) return [];
    // (isEmptyStream(stream)) return stream;
    const [value, lazyTail] = stream;
    return [value, ...(take(force(lazyTail), total - 1))];
}

const map = <T, R>(stream: Stream<T>, f: (t: T) => R): Stream<R> => {
    if (isEmptyStream(stream)) return stream;
    const [head, lazyTail] = stream;
    return cons(f(head), () => map(force(lazyTail), f));
}
```

测试我们写的常规操作

```typescript
const ones10_1 = take(ones, 10);
const ones10_2 = take(ones, 10);
```

上述代码会报错。

![image-20201226233257351](/home/bestmasterliubin/dev/sicp/sicp阅读笔记/JavaScript中的惰性求值与流.assets/image-20201226233257351.png)

为什么会报错呢？我们第二次执行 take(ones, 10)，ones = [ 1, lazy(undefined) ]，因为我们的 lazy 函数是一次性，force 一次后，第二次force返回的是 undefined，这显然不合理。

继续改善我们的 lazy 函数

```typescript
function lazy<T>(value: () => T): Lazy<T> {
    return (function *() {
        while (true) {
            yield value();
        }
    })();
}
```

加入 while 死循环后我们的 lazy 就不是一次性的了，对于同一个 lazy 值，无论force执行多少次，拿到的值都是相同。

接下来我们分析下面代码的执行过程

```typescript
const integers = infinityStream(1, v => v + 1);
const integersMultiply2 = map(ones, v => v * 2);
take(mapedIs, 10);
```

1. 生成自然数流初始值：``integers: [1, Lazy([2, Lazy<...>])]``
2. 执行 map：``integersMultiply2: cons( f(1), () => map([2, Lazy<...>], f) )``, 其实这里我们就看到了，map 函数将 integers 的一个元素转换为 2 后，实际上就没有干什么事，什么时候对 integers 的剩余元素，执行函数 f 呢？
3. 执行 take: ``take: [2, Lazy(map([2, Lazy<...>], f))], 10``， 拿到 2，递归进行 take
4. 执行 take第二次，``stream = force(Lazy( map([2, lazy<...>], f))) ``, 当我们想要拿到 integersMultiply2 流中的第二个元素时，这时 map 中的 f 才开始执行第二次。拿到 f(2) = 4
5. ...

这样我们就观察到执行 map(integers, f) 时，integers 不会马上对所有元素执行 f, 而是在你需要这个元素的时候才执行对应的 f。

## 更多的例子

先补充我们的工具函数，

```typescript
// filter 函数在内部循环执行直到找到一个符合条件的元素后，才会等待下一次执行
// 若你对一个无限流使用 filter 并且传入一个永远不会成功的判断函数，就会造成死循环
const filter = <T>(stream: Stream<T>, predicate: (t: T) => boolean): Stream<T> => {
    if (isEmptyStream(stream)) return stream;
    let [value, lazyTail] = stream;
    if (predicate(value)) return cons(value, () => filter(force(lazyTail), predicate));
    return filter(force(lazyTail), predicate);
}


import {add} from "ramda";

const addStream = (s1: Stream<number>, s2: Stream<number>): Stream<number> => cons(add( car(s1), car(s2)), () => addStream(cdr(s1), cdr(s2)));
```

造更多的无限流

```typescript
// 斐波那契数列流
const fibStream: number, b: number): Stream<number> => cons(a, () => fibStream(b, a + b));
const fib = fibStream(0, 1);

import {not} from "ramda";

// 埃拉托斯特尼筛法构造素数流
// 1. 首先传入的是 [2, lazy([3,4,5,6,..])] 
// 2. 第二次传入的是对 filter([3,4,5,6,..], 是否不能整除2)
// 3. 第三次传入的是对 filter([5,7,9,11,13,..], 是否不能整除3)
// ...
const divisible = (a: number, b: number) => a % b === 0;
const sieve = (stream: Stream<number>): Stream<number> =>
    cons(car(stream), () => sieve(filter(cdr(stream), x => not(divisible(x, car(stream))))));
const primes = sieve(infinityStream(2, v => v + 1));

// 直接定义无限流
const ones = cons(1, () => ones);

const integers = cons(1, () => addStream(ones, integers));

const fibs = cons(0, () => cons(1, () => addStream(cdr(fibs), fibs)));
```

console.log(take(fib, 100));

![image-20201227003415266](/home/bestmasterliubin/dev/sicp/sicp阅读笔记/JavaScript中的惰性求值与流.assets/image-20201227003415266.png)

console.log(take(primes, 100));

console.log(take(ones, 10));

console.log(take(integers, 10));

console.log(take(fibs, 10));

![image-20201227003542313](/home/bestmasterliubin/dev/sicp/sicp阅读笔记/JavaScript中的惰性求值与流.assets/image-20201227003542313.png)

==可以看到流的核心在于 ``cons`` 流的构造函数，第二参数传入函数，函数并不是立即执行的，而是等你需要用到该元素时，它才会将该元素构造出来。==

还有更多例子就不一一演示了。



















