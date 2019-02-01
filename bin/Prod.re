module type Monoid = {
  type t;
  let append: (t, t) => t;
};

module SumInt = {
  type t = int;
  let append = (n1, n2) => n1 + n2;
};

module ProdInt = {
  type t = int;
  let append = (n, n) => n * n;
};

let sum = (type a, module M: Monoid with type t = a, n: a): a => {
  M.append(n, n);
};

let () = {
  let res1 = sum((module SumInt), 10);
  print_string("Sum " ++ string_of_int(res1) ++ "\n");
  let res2 = sum((module ProdInt), 10);
  print_string("Prod " ++ string_of_int(res2) ++ "\n");
};
