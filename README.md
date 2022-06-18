# Plutus JavaScript Bridge

A Haskell package and CLI application to help guarantee valid datum/redeemer
creation in JavaScript for client-side transaction construction.


## Table of Contents
- [What is it?](#what-is-it)
  - [Limitation](#limitation)
    - [Sum Types vs. Product Types](#sum-types-vs-product-types)
    - [The Limitation](#the-limitation)
- [Quickstart](#quickstart)
  - [Prerequisites](#prerequisites)
  - [Steps](#steps)
  - [Notes on Sample Values](#notes-on-sample-values)
- [How to Use the Generated Functions](#how-to-use-the-generated-functions)
- [Generating from JSON Files](#generating-from-json-files)
- [Future](#future)


## What is it?

[//]: # ({{{)

A Plutus smart contract has a validation logic at its core. This validation
relies on datums attached to UTxO's, and a redeemer (both being arbitrary
data). When developed in Haskell, a more maintainable approach calls for
custom datatypes for both of these values.

For a web dApp, it's sometimes desirable to construct a transaction inside
user's browser and ask for their signature via their browser wallet. This
typically means that the constructed transaction is consuming a UTxO from
user's wallet, and sending a new one to the script address of your smart
contract. Therefore, this leads to some values getting "locked" into the
contract.

A successful consumption of this locked value requires approval by contract's
validation logic, which can potentially rely on a datum attached to the input
UTxO's. If this attached datum is not properly structured, the validation logic
can fail and lead to unretrievable values.

So basically, users rely on your client-side program to attach a proper and
valid datum to the UTxO's at the output of the transaction which they initially
signed.

This tool is designed to help guarantee proper formatting of your
datum/redeemer values in your client-side code by taking in datum/redeemer
samples of your on-chain validator, and generating JavaScript functions for
your frontend logic to utilize.


### Limitation

To better understand the limitation of this tool, let's first take a detour
into Haskell datatypes.

#### Sum Types vs. Product Types

[//]: # ({{{)

In Haskell, datatypes are either *sum types*, or *product types* (also called
*record types*).

Logically, sum types are equivalent to the **OR** logic, while product types
represent the **AND** logic.

A `Bool` is one of the simplest of sum types: its values can either be `True`
**OR** `False`.

On the other hand, a *tuple* is a simple product type, because its values
consist of the values of its first element **AND** the values of its second
element.

To better demonstrate the *product* here, let's define another sum type:
```hs
data Color
  = Red
  | Green
  | Blue
```

`Color` can have 3 values. How many values can a tuple of a `(Bool, Color)`
have?
```hs
(True , Red)
(True , Green)
(True , Blue)
(False, Red)
(False, Green)
(False, Blue)
```

We can see that there are 6 possible values, which is the the *product* of 2
and 3. Similarly, a triple of `(Bool, Color, Color)` can have `2 x 3 x 3`
number of distinct values.

A record type is just a tuple (or triple, or quarduple, etc.), the only
difference is that its values are labeled. So we could define our tuple a
bit more explicitly:
```hs
data QuantumColor =
  QuantumColor { qcIsReal :: Bool
               , qcColor  :: Color
               }
```

If you respect the order of the fields, you can construct a `QuantumColor`
value without specifying the handles. Both of these are valid:
```hs
trueRed :: QuantumColor
trueRed =
  QuantumColor True Red

falseBlue :: QuantumColor
falseBlue =
  QuantumColor { qcIsReal = False
               , qcColor  = Blue
               }
```

[//]: # (}}})

#### The Limitation

[//]: # ({{{)

The high-level goal of this tool is to re-create arbitrary Haskell datatypes in
JavaScript. We just saw that a value of a record type in Haskell can be
constructed without specifying the field handles, and simply by respecting the
order of the fields. This means that JavaScript's arrays can be good candidates
to represent a record type. So for example, a record type with 3 fields can be
represented by a 3-element array in JavaScript.

On the other hand, a Haskell sum type does not have a simple equivalent in
JavaScript. Let's demonstrate this with another common Haskell sum type,
`Maybe`:
```hs
data Maybe a
  = Nothing
  | Just a
```

As an example, a `Maybe Integer` can either be `Nothing`, or `Just 42` (or any
other number). To define this datatype in JavaScript, you should either define
a custom object, or somehow resort to string literals. In other words, there is
no simple construct suited for this representation.

What you can do, is to define seperate constructor functions for each variant:
```js
function makeNothing() {...}
function makeJustInt(num) {...}
// We'll cover what these should return later.
```

A `Maybe Integer` is kinda easy though... How about `Maybe Color`? We're gonna
need more functions to be able to cover all the possible values:
```js
function makeNothing() {...}
function makeJustRed() {...}
function makeJustGreen() {...}
function makeJustBlue() {...}
```

This can easily grow out of hand. Let's define another sum type to better see
this:
```hs
data MultiColors
  = NoColor
  | OneColor Color
  | TwoColors Color Color
```

The corresponding JavaScript functions will be:
```js
function makeNoColor() {...}
function makeOneRed() {...}
function makeOneGreen() {...}
function makeOneBlue() {...}
function makeTwoReds() {...}
function makeARedAndAGreen() {...}
// and so on...
```

You can see how the number of generated functions can grow rapidly with nesting
of sum types. This is the limitation that requires your careful data modeling.

There are of course other frameworks available for JavaScript that are capable
of representing sum types (PureScript, Elm, etc.). But this tool aims to allow
bridging for pure JavaScript. This does not seem to be an unsolvable
limitation. So hopefully, it will be addressed in future updates.

[//]: # (}}})

[//]: # (}}})


## Quickstart

[//]: # ({{{)

Fastest and simplest way to generate JavaScript functions is to use the Haskell
package in this repository.


### Prerequisites

[//]: # ({{{)

This package itself relies on [`plutus-apps` repository](https://github.com/input-output-hk/plutus-apps),
which is very likely one of your project's primary dependencies already.

Generated functions from this tool require two things:
- [Emurgo's serialization library](https://github.com/Emurgo/cardano-serialization-lib),
- and a set of helper functions defined in `js/helpers.js`.

You can learn more about the `PlutusData` object (which is the object returned
by the generated functions) from the serialization library through
[its docs](https://input-output-hk.github.io/cardano-js-sdk/classes/_cardano_sdk_core.CSL.PlutusData.html).

[//]: # (}}})


### Steps

[//]: # ({{{)

1. If you don't already have the `plutus-apps` repository, clone it at a custom
   directory:
   ```bash
   $ git clone https://github.com/input-output-hk/plutus-apps.git
   ```

2. Clone this bridge repository, preferably next to your own project:
   ```bash
   $ git clone https://github.com/snapbrillia/plutus-js-bridge.git
   ```
   For easier referencing of relative paths, this is the directory structure
   this guide assumes:
   ```bash
   $ ls
   plutus-apps  plutus-js-bridge  your-plutus-project
   ```

3. (Optional) It is very likely that you already have a `dist-newstyle`
   folder in `your-plutus-project`. This is the build result from your
   dependencies, many of which are probably shared with this package. You can
   utilize a symlink from your project to avoid redownloding and rebuilding
   those dependencies:
   ```bash
   $ ln -s your-plutus-project/dist-newstyle plutus-js-bridge/dist-newstyle
   ```

4. Edit `your-plutus-project/cabal.project` file to include the bridge package.
   Add another element to its `packages` field that points to
   `plutus-js-bridge.cabal`. For example:
   ```haskell
   packages:   your-plutus-project.cabal
             , ../plutus-js-bridge/plutus-js-bridge.cabal
   ```

5. Edit `your-plutus-project/your-plutus-project.cabal` file and add this
   `executable`stanza:
   ```haskell
   executable your-plutus-project-bridge-app
     main-is:              bridge.hs -- <-- Change the file name if you already have one named as such,
     hs-source-dirs:       app       -- <-- and note that you should have this folder.
     build-depends:        base >= 4.9 && < 5
                         , plutus-js-bridge
                         , your-plutus-project
   ```

6. This is a suggested `app/bridge.hs` from step 5:
   ```haskell
   {-# LANGUAGE OverloadedStrings #-}

   module Main where

   import qualified PlutusBridge

   -- import data constructors of your custom datum/redeemer.
   import YourOnChainModule (MyDatum (..), MyRedeemer (..))

   main :: IO ()
   main = do
     PlutusBridge.run
       "js/datums.js"   -- <-- Output file containing the generated functions.
       [ ( "makeDatumA" -- <-- Name of the function that creates a datum structures as `Datum1 42`.
         , DatumA 42    -- <-- A sample value to help the generator deduce its structure.
         )
       , ( "makeDatumB"
         , DatumB "hello"
         )
       ]
     PlutusBridge.run
       "js/redeemers.js"
       [ ( "makeRedeemerA"
         , RedeemerA 43
         )
       , ( "makeRedeemerB"
         , RedeemerB "world"
         )
       ]
   ```
   Change the import module, output JavaScript files, function names and sample
   values accordingly (see [Notes on Sample Values](#notes-on-sample-values)).

7. Enter a Nix shell from within `plutus-apps`:
   ```bash
   $ cd plutus-apps
   $ nix-shell
   ```

8. Return to `your-plutus-project`, and run your new executable:
   ```bash
   $ cd ../your-plutus-project
   $ cabal run your-plutus-project-bridge-app
   ```
   `your-plutus-project-bridge-app` is the name we defined in 
   `your-plutus-project.cabal` file's `executable` stanza.

You should have two generated JavaScript files now: `js/datums.js` and
`js/redeemers.js`.

`js/datums.js` looks like this:
```js
export function makeDatumA(...){...}
export function makeDatumB(...){...}
```

Now you can import the generated functions into your frontend application via
these files.

[//]: # (}}})


### Notes on Sample Values

[//]: # ({{{)

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

See [Limitation](#limitation) for more detailed explanations.

[//]: # (}}})

[//]: # (}}})


## How to Use the Generated Functions

[//]: # ({{{)

For consistency and convenience, the generated functions are designed to work
with string values, and/or (possibly nested) lists of strings.

Imagine a custom datum defined as a sum type:
```haskell
data MyDatum
  = PointDatum Integer Integer
  | PathDatum [(Integer, Integer)]
```

For this datum, you should provide two sample values (see
[Sum Types vs. Product Types](#sum-types-vs-product-types) for more info):
```haskell
main = do
  -- ...
  PlutusBridge.run "js/datums.js"
    [ ("makePointDatum", PointDatum 0 0)
    , ("makePathDatum" , PathDatum [(0, 0)])
    ]
  -- ...
```

This generates `js/datums.js` with two function definitions: `makePointDatum`,
which expects two string values (*that can be translated into integers*), and
`makePathDatum` which expects an array of two-element arrays.

After importing `js/datums.js` ([and `helpers.js`](#prerequisites)) into your
JavaScript code, you can call these functions like this:
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

[//]: # (}}})


## Generating from JSON Files

[//]: # ({{{)

From an automation standpoint, it might be easier to have simple executable
binaries in your `PATH` to work with rather than incorporating a build process
into your pipeline. This package comes with a Haskell application dedicated to
work independently with JSON files to generate equivalent JavaScript functions
for the serialization library.

1. You should already have a scheme in place for generating your custom
   datum/redeemer values as JSON files (using the `cardano-api` library). This
   code (which, for the most part, is taken from
   [the `plutus-pioneer-program` repository](https://github.com/input-output-hk/plutus-pioneer-program)),
   is a good example of the kind of code that you should have in your program:
   ```haskell
   module GenerateJSON (writeJSON) where
   
   import           Cardano.Api
   import           Data.Aeson            (encode)
   import qualified Data.ByteString.Lazy  as LBS
   import           PlutusTx              (Data (..))
   import qualified PlutusTx
   
   dataToScriptData :: Data -> ScriptData
   dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
   dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
   dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
   dataToScriptData (I n)         = ScriptDataNumber n
   dataToScriptData (B bs)        = ScriptDataBytes bs
   
   writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
   writeJSON file =
       LBS.writeFile file
     . encode
     . scriptDataToJson ScriptDataJsonDetailedSchema
     . dataToScriptData
     . PlutusTx.toData
   ```
   Upon execution of your application, it should generate JSON files for
   various instances of your custom datum/redeemer datatypes. This is a
   valid `Main` module for such a JSON generator program:
   ```haskell
   module Main where
   
   import GenerateJSON

   -- import data constructors of your custom datum/redeemer.
   import YourOnChainModule (MyDatum (..), MyRedeemer (..))

   main :: IO ()
   main = do
     writeJSON "json/makeDatumA.json"    $ DatumA 42
     writeJSON "json/makeDatumB.json"    $ DatumB "hello"
     writeJSON "json/makeRedeemerA.json" $ RedeemerA 43
     writeJSON "json/makeRedeemerB.json" $ RedeemerB "world"
   ```
   Please take a look at [Notes on Sample Values](#notes-on-sample-values) to
   learn more about providing samples for your application.

2. Follow the first 3 steps from [the quickstart section](#steps).

3. Install the bridge application:
   ```bash
   $ cd plutus-js-bridge
   $ cabal install plutus-bridge-app
   ```

4. The application expects path of the output JavaScript file as its first
   argument, followed by its reference JSON files for function generation:
   ```bash
   $ plutus-bridge-app js/datums.js json/makeDatumA.json json/makeDatumB.json
   ```

   Note that the names of the JSON files are used as the names of the generated
   functions.

   So in the example above, `js/datums.js` will have such contents:
   ```js
   export function makeDatumA(...){...}
   export function makeDatumB(...){...}
   ```

[//]: # (}}})


## Future

Solving [the limitation](#limitation) mentioned earlier is a potential future
upgrade to this module.


