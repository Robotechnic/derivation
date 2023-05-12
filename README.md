# derivation

A OCaml program to derivate functions.

## Usage

```sh
derivation [-v VARIABLE] [-e EXPRESSION] [-h]
```

### Options

| Option | Description | Default |
| :----: | :---------: | :-----: |
| -v     | Variable to derivate |
| -e     | Expression to derivate |
| -h     | Show help |

### Default

If no option is given, the program will read from stdin. And the default derivation variable is `x`.

## Examples

```sh
$ derivation -v x -e "x^2 + 2*x + 1"
2x + 2
$ derivation -v x -e "x^2 + 2*x + 1" | derivation -v x
2
$ derivation -v x -e "(x*y)^2 + 2 * x * y + 1" | derivation -v y
2 * x * y + 2y * x + 2
```
