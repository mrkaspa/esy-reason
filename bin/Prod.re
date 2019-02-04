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

let crossProduct = (v1, v2) => {
  let (a1, a2, a3) = v1;
  let (b1, b2, b3) = v2;

  (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1);
};

let () = {
  let res1 = sum((module SumInt), 10);
  print_string("Sum " ++ string_of_int(res1) ++ "\n");
  let res2 = sum((module ProdInt), 10);
  print_string("Prod " ++ string_of_int(res2) ++ "\n");

  if (res1 == res2) {
    print_string("EQ\n");
  };
};
