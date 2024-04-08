```
String ::= {"A" | ... | "Z" | "a" | ... | "z"}-

Exp ::= Int             (Num)
| (+ Exp Exp)           (Plus)
| (* Exp Exp)           (Mult)
| true                  (True)
| false                 (False)
| (and Exp Exp)         (And)
| (or Exp Exp)          (Or)
| (not Exp)             (Not)
| (if Exp Exp Exp)      (If)
| (<=> Exp Exp)         (BiImpl)
| (=> Exp Exp)          (Impl)

| (str String)          (string)
| (strEquals Exp Exp)   (strEquals)
| (charAt Exp Exp)      (charAt)
| (head Exp)            (head)
| (startsWith Exp Exp)  (startsWith)
| (endsWith Exp Exp)    (endsWith)
| (concat Exp Exp)      (concat)
| (length Exp)          (length)
```
