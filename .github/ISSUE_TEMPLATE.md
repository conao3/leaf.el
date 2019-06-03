## Description

[Please write a description of your issue]


=== [Please delete the following section if it is not relevant] === 

## Issue leaf-block
```
(leaf hoge
  :config (hoge-init))
```

## Expected output from macroexpand-1 of the leaf-block
```
(prog1 'hoge
  (leaf-handler-leaf-protect hoge
    (hoge-init)))
```
