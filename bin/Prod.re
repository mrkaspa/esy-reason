module type Sum = {
  type t;
  let sumi: (t, t) => t;
};

module SumInt = {
  type t = int;
  let sumi = (n, n) => n + n;
};

let sum = (type a, module M: Sum with type t = a, n: a): a => {
  M.sumi(n, n);
};

let () = {
  let res = sum((module SumInt), 10);
  print_string("Demo " ++ string_of_int(res) ++ "\n");
};
