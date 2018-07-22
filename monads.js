const compose = (f, g) => ((x) => f(g(x)));
const addThree = (x) => x + 3;
const addFive = (x) => x + 5;
const addEight = compose(addThree, addFive);

console.log("regular composition", addEight(5));

// class Monad m where
//   (>>=) :: m a -> (a -> m b) -> m b
//   return :: a -> m a


class Reader {
  constructor(f) {
    this.f = f;
  }

  bind(b) {
    return new Reader(compose(b, this.f));
  }

  static return(a) {
    return new Reader(() => a);
  }

  run() {
    return this.f();
  }
}

const monadic = Reader.return(5).bind(addThree).bind(addFive).run();
console.log("monadic composition", monadic);

class List {
  constructor(list) {
    this.list = list;
  }

  bind(b) {
    // join . fmap
    return new List(this.list.map((a) => b(a)).reduce((acc, curr) => (acc.concat(curr)), []));
  }

  static return(a) {
    return new List([a]);
  }

  unwrap() {
    return this.list;
  }
}

const monadicList = new List([1, 2, 3]).bind((a) => ([addThree(a)]));
console.log(monadicList.unwrap());const compose = (f, g) => ((x) => f(g(x)));
const addThree = (x) => x + 3;
const addFive = (x) => x + 5;
const addEight = compose(addThree, addFive);

console.log("regular composition", addEight(5));

// class Monad m where
//   (>>=) :: m a -> (a -> m b) -> m b
//   return :: a -> m a


class Reader {
  constructor(f) {
    this.f = f;
  }

  bind(b) {
    return new Reader(compose(b, this.f));
  }

  static return(a) {
    return new Reader(() => a);
  }

  run() {
    return this.f();
  }
}

const monadic = Reader.return(5).bind(addThree).bind(addFive).run();
console.log("monadic composition", monadic);

class List {
  constructor(list) {
    this.list = list;
  }

  bind(b) {
    // join . fmap
    return new List(this.list.map((a) => b(a)).reduce((acc, curr) => (acc.concat(curr)), []));
  }

  static return(a) {
    return new List([a]);
  }

  unwrap() {
    return this.list;
  }
}

const monadicList = new List([1, 2, 3]).bind((a) => ([addThree(a)]));
console.log(monadicList.unwrap());