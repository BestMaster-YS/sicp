type Wire = {
  getSignal: () => elec;
  setSignal: (value: elec) => Promise<void>;
  addAction: (action: () => Promise<void>) => void;
}

type elec = 0 | 1;

let time = +new Date();

function wire(name: string): Wire {
  let procedures: (() => Promise<void>)[] = [];
  let signal: elec = 0;
  return {
    getSignal: () => signal,
    setSignal: async (value: elec) => {
      signal = value;
      console.log('now time: ' + Math.floor((+new Date() - time + 300) / 1000));
      console.log(name, signal);
      await Promise.all(procedures.map(async (proc) => await proc()));
    },
    addAction: (action) => {
      procedures.push(action);
    }
  };
}

function inverter(input: Wire, output: Wire) {
  input.addAction( () => new Promise((resolve) =>{
    setTimeout(async () => {
      resolve();
      await output.setSignal(not(input.getSignal()));
    }, 2000)
  }))
}

function andGate(input1: Wire, input2: Wire, output: Wire) {
  input1.addAction(() => new Promise((resolve) =>{
    setTimeout(async () => {
      resolve()
      await output.setSignal(and(input1.getSignal(), input2.getSignal()));
    }, 5000)
  }))
  input2.addAction(() => new Promise((resolve) =>{
    setTimeout(async () => {
      resolve();
      await output.setSignal(and(input1.getSignal(), input2.getSignal()));
    }, 5000)
  }))
}

function orGate(input1: Wire, input2: Wire, output: Wire) {
  input1.addAction( () => new Promise((resolve) =>{
    setTimeout(async () => {
      resolve();
      await output.setSignal(or(input1.getSignal(), input2.getSignal()));
    }, 3000)
  }));
  input2.addAction( () => new Promise((resolve) =>{
    setTimeout(async () => {
      resolve();
      await output.setSignal(and(input1.getSignal(), input2.getSignal()));
    }, 3000)
  }));
}


const not = (v: 0 | 1) => v === 0 ? 1 : 0;
const or = (v1: 0 | 1, v2: 0 | 1) => (v1 === 1 || v2 ===  1) ? 1 : 0;
const and = (v1: 0 | 1, v2: 0 | 1) => (v1 === 1 && v2 ===  1) ? 1 : 0;


function halfGate(a: Wire, b: Wire, s: Wire, c: Wire) {
  const d = wire('d');
  const e = wire('e');
  orGate(a, b, d);
  andGate(a, b, c);
  inverter(c, e);
  andGate(d, e, s);
}


const a = wire('a');
const b = wire('b');
const s = wire('sum');
const c = wire('c');

halfGate(a, b,s,c)
a.setSignal(1).then(() => {
  b.setSignal(1);
});
