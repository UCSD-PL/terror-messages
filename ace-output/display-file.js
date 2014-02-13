var arraySrc = [
  "",
  "foo :: Int -> Int",
  "foo 0 = 1",
  "foo 1 = 1",
  "foo n = n * (foo $ n-1)",
  "",
  "map :: (a -> b) -> [a] -> [b]",
  "map f []     = []",
  "map f (x:xs) = f x : map f xs",
];

editor.setValue(arraySrc.join("\n"));
editor.gotoLine(0);

clearAnnotations();

addAnnot(5, 14, "bool -> bool");
addAnnot(9, 22, "(a -> b) -> [a] -> [b]");
