# Plutus JavaScript Bridge

A Haskell package and CLI application to help guarantee identical
datum/redeemer creation in JavaScript such that they match your on-chain
datatypes.


## Limitation

Since Haskell's sum types don't have simple representations in Javascript, a
separate function should be generated for each data constructor of a sum type.
Therefore, if nested sum types are present, the number of
[sample values](#notes-on-sample-values) that you should provide for the
application can grow exponentially.

Solving this limitation would require a "constructor selection" scheme, e.g.
identifying the intended constructor with an extra argument in the generated
function. This extra argument can be a string literal that matches the data
constructor from Haskell. This, however, is prone to typos and similar faults.


## Primitive Helper Functions

Generated functions from this package rely upon a few helper functions defined
in `js/helpers.js`. Make sure to account for their inclusion in your bundling
pipeline such that the generated functions have access to them.


## Generating JS Functions

This package requires the `plutus-tx` library, so the best way to build it
is to enter a `nix-shell` from the
[`plutus-apps` repository](https://github.com/input-output-hk/plutus-apps):

  1. If you don't already have the repo, clone it at a custom directory:
     ```bash
     $ git clone https://github.com/input-output-hk/plutus-apps
     $ cd plutus-apps
     ```

  2. Enter Nix shell:
     ```bash
     $ nix-shell
     ```

  3. Enter this repo:
     ```bash
     $ cd ~/plutus-js-bridge
     ```

  4. (Optional) It is very likely that you already have a `dist-newstyle`
     folder in your Plutus project directory. This is the build result from
     your dependencies, many of which are probably shared with this repo's.
     You can utilize a symlink from your project to prevent redownloding and
     rebuilding those dependencies:
     ```bash
     $ ln -s ~/path/to/my/plutus/project/dist-newstyle ~/plutus-js-bridge/dist-newstyle
     ```

There are two possible paths to take from here:

### 1. Importing the Library

  5. In your `cabal.project` file, add another element to its `packages` field
     that points to this repo's `.cabal` file, e.g.:
     ```
     packages:   myPlutusProject.cabal
               , ../plutus-js-bridge/plutus-js-bridge.cabal
     ```

  6. Add `plutus-js-bridge` to your project's `build-depends` in its
     cabal file.

  7. Import the `PlutusBridge` module into one of your applications, and call
     `PlutusBridge.run` in its `main`. This function expects the output file,
     and a list of tuples—a mapping from the target JavaScript function name
     to a sample value of your custom datum/redeemer.  Refer to the
     [notes below](#notes-on-sample-values) for details about sample values.

     Note that you should have `OverloadedString` extension activated in this
     `main.hs` file.

### 2. Using the Builtin Application to Parse JSON of Datum Values

  5. You should already have a scheme in place for generating your custom
     datum/redeemer values as JSON files (using the `cardano-api` library).
     Refer to the [notes below](#notes-on-sample-values) for details about
     sample values.

  6. You can either build the application into a binary:
     ```bash
     $ cabal build plutus-bridge-app
     $ plutus-bridge-app outputFile.js makeDatum1.json makeDatum2.json
     ```
     or simply execute on demand:
     ```bash
     $ cabal run plutus-bridge-app -- outputFile.js makeDatum1.json makeDatum2.json
     ```

     The expected arguments are the output JavaScript file, followed by a list
     of JSON files, where their filenames are used as the names of their
     repective generated functions. For instance, `path/to/makeDatum1.json`
     would result in a function named `makeDatum1` in JavaScript.


## Notes on Sample Values

Keep in mind to make your sample values as inclusive as possible to allow the
generator deduce their complete structure. For example, if your datum carries
a list, make sure that you provide at least one element inside it. The values
themselves don't matter, they are only used to figure out the structure.

Another thing to note, is that while Haskell's record types can map to
JavaScript arrays (_where ordering matters_), sum types can be viewed as
"branching points" since they can't be converted to simple JavaScript
constructs. Therefore a separate function should be generated for every
combination of present sum types. Which is why nested sum types can lead to an
exponential growth in the number of generated functions.


## Using the Generated JS Functions

For consistency and convenience, the generated functions are designed to work
with string values, and/or (possibly nested) lists of strings.

Imagine a custom datum defined as such:
```haskell
data MyDatum
  = PointDatum Integer Integer
  | PathDatum [(Integer, Integer)]
```

For this datum, you should provide two sample values:
```haskell
main = do
  -- ...
  PlutusBridge.run "js/bridge.js"
    [ ("makePointDatum", PointDatum 0 0)
    , ("makePathDatum" , PathDatum [(0, 0)])
    ]
  -- ...
```

This generates `js/bridge.js` with two function definitions: `makePointDatum`,
which expects two string values that can be translated into integers, and
`makePathDatum` that expects an array of two-element arrays:
```js
console.log
  ( makePointDatum
      ( "42"
      , "13"
      )
  );

console.log
  ( makePathDatum
      ( [ ["0", "0"]
        , ["1", "1"]
        , ["2", "4"]
        ]
      )
  );
```


## Incorporating Into Your Build Pipeline

If you are using the library approach, you should embed the generator
application (the `executable` with `PlutusBridge.run` in its `main`) into your
build pipeline.

For the JSON approach, you'll also need to preface the execution of this
application with your other application that generates sample JSON files.

As a simple example with `npm`, you can edit the `start` script from
`package.json` to include the executable that generates sample JSON files,
followed by `plutus-bridge-app` with appropriate arguments:
```bash
generate-sample-jsons && plutus-bridge-app js/bridge.js makeDatum1.json makeDatum2.json && ...
```

[Just keep in mind that you should incorporate `js/helpers.js` either way](#primitive-helper-functions).



