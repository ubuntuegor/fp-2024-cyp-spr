# lil - L Interpreted Language

## How to run:
```shell
stack run -- -h # Show help
```

```shell
stack run -- -i program.lil # Run a program
```

```shell
stack run -- -i program.lil -o output.txt # Run a program and write output to file
```

## Example program
```js
function gcd(a, b) {
  if (b == 0) {
    a
  } else {
    gcd(b, a % b)
  }
}

read a;
read b;

write gcd(a, b)
```
