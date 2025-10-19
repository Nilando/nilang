// Without any format options for printing I think this is as close as I can get
// for now. Still this is not a quine

a = fn (x) { return `{x}print(a("{x}"));`; };print(a('a = fn (x) {return`{x}print(a("{x}"));`; };'));
