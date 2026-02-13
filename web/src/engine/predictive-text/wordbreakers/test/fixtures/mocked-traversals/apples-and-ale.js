const mockedIterator = function*() {};

const apple = {
  children: mockedIterator,
  child: () => {},
  entries: [{
    text: 'apple',
    p: .09
  }],
  maxP: .09
};

const apply = {
  children: mockedIterator,
  child: () => {},
  entries: [{
    text: 'apply',
    p: .01
  }],
  maxP: .01
};

const appl = {
  children: mockedIterator,
  child: (char) => {
    if(char == 'e'){
      return apple;
    } else if(char == 'y') {
      return apply;
    } else {
      return undefined;
    }
  },
  entries: [],
  maxP: .09
}

const apps = {
  children: mockedIterator,
  child: () => {},
  entries: [{
    text: 'apps',
    p: .13
  }],
  maxP: .13
};

const app = {
  children: mockedIterator,
  child: (char) => {
    if(char == 'l'){
      return appl;
    } else if(char == 's') {
      return apps;
    } else {
      return undefined;
    }
  },
  entries: [{
    text: 'app',
    p: .07
  }],
  maxP: .13
}

const ap = {
  children: mockedIterator,
  child: (char) => {
    if(char == 'p'){
      return app;
    } else {
      return undefined;
    }
  },
  entries: [],
  maxP: .13
}

const ale = {
  children: mockedIterator,
  child: () => {},
  entries: [{
    text: 'ale',
    p: .03
  }],
  maxP: .03
};

const all = {
  children: mockedIterator,
  child: () => {},
  entries: [{
    text: 'all',
    p: .11
  }],
  maxP: .11
};

const al = {
  children: mockedIterator,
  child: (char) => {
    if(char == 'e'){
      return ale;
    } else if(char == 'l') {
      return all;
    } else {
      return undefined;
    }
  },
  entries: [],
  maxP: .11
}

const and = {
  children: mockedIterator,
  child: () => {},
  entries: [{
    text: 'and',
    p: .25
  }],
  maxP: .25
};

const any = {
  children: mockedIterator,
  child: () => {},
  entries: [{
    text: 'any',
    p: .04
  }],
  maxP: .04
};

const an = {
  children: mockedIterator,
  child: (char) => {
    if(char == 'd'){
      return and;
    } else if(char == 'y') {
      return any;
    } else {
      return undefined;
    }
  },
  entries: [{
    text: 'an',
    p: .06
  }],
  maxP: .25
}

const a = {
  children: mockedIterator,
  child: (char) => {
    if(char == 'n'){
      return an;
    } else if(char == 'l') {
      return al;
    } else if(char == 'p') {
      return ap;
    } else {
      return undefined;
    }
  },
  entries: [{
    text: 'a',
    p: .15
  }],
  maxP: .25
}

const root = {
  children: mockedIterator,
  child: (char) => {
    if(char == 'a'){
      return a;
    } else {
      return undefined;
    }
  },
  maxP: .25
}

export const fixture = root;