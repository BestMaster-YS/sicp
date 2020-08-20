type ComputedValue = number | string | Array<any> | { [key: string]: any } | boolean | Symbol | null;
type LazyComputedValue = () => ComputedValue;
type LazyItem =  [ComputedValue, LazyComputedValue];

const memorize = (fn: (...args: any[]) => ComputedValue) => {
  let alreadyRun: boolean = false;
  let result: ComputedValue;
  return (...args: any[]): ComputedValue => {
    if (alreadyRun) {
      return result;
    }
    result = fn(...args);
    alreadyRun = true;
    return result;
  }
}


const lazy: ((v: ComputedValue) => LazyComputedValue) = (v: ComputedValue) => memorize(() => v);

const car = (tuple: LazyItem) => tuple[0];
const cdr = (tuple: LazyItem) => tuple[1]();
const cons = (v1: ComputedValue, v2: ComputedValue): LazyItem => [v1, lazy(v2)]

const generateStreamInterval = (low: number, high: number) => {
  if (low === high) return memorize(() => high);
  return [low, () => generateStreamInterval(low + 1, high)]
}


function generateIntegersStream() {
  let sum = 0;
  function* integers() {
    while (true) {
      yield 1;
    }
  }

  return () => {
    sum += (integers().next().value as number) ?? 0;
    return sum;
  };
}



const a = generateIntegersStream();

console.log(a())
console.log(a())
console.log(a())
