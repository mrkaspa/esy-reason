open Core;

type parserResult('a) = (string, result('a, string));

type parser('a) = string => parserResult('a);

let parseSymbol = (pattern, input) => {
  let size = String.length(pattern);
  let sub = String.sub(input, ~len=size, ~pos=0);
  if (pattern == sub) {
    let rest =
      String.sub(input, ~len=String.length(input) - size, ~pos=size);
    (rest, Ok(sub));
  } else {
    (input, Error(input));
  };
};

let pair = (p1: parser('a), p2: parser('b)): parser(('a, 'b)) => {
  input => {
    switch (p1(input)) {
    | (rest1, Ok(res1)) =>
      switch (p2(rest1)) {
      | (rest2, Ok(res2)) => (rest2, Ok((res1, res2)))
      | _ => (input, Error(input))
      }
    | _ => (input, Error(input))
    };
  };
};

let runParser = (parser: parser('a), input) => parser(input);

let exec = (~token_parser: 'a => string=a => "no", parser: parser('a), cad) => {
  let (rest, res) = runParser(parser, cad);
  print_string("the rest is " ++ rest ++ "\n");
  switch (res) {
  | Ok(token) =>
    let printable = token_parser(token);
    print_string("token found " ++ printable ++ "\n");
  | Error(_) => print_string("token not found \n")
  };
};

let ident = a => a;

let _ = {
  let parserA = parseSymbol("a");
  exec(parserA, "a house", ~token_parser=ident);
  /* combined */
  let parserB = parseSymbol("b");
  let parserAB = pair(parserA, parserB);
  exec(parserAB, "ab house", ~token_parser=((tok1, tok2)) =>
    tok1 ++ " - " ++ tok2
  );
};
