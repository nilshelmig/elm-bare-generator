# elm-bare-generator
A CLI tool to generate elm code from [BARE](https://baremessages.org/) schema by using [elm-bare](https://package.elm-lang.org/packages/miniBill/elm-bare/latest).

## Usage
1. Install [elm-bare](https://package.elm-lang.org/packages/miniBill/elm-bare/latest) for your elm project
```sh
elm install miniBill/elm-bare
```
2. Call elm-bare-generator to generate an elm module which exposes types and functions to handle BARE messages
```sh
npx elm-bare-generator <input schema> <output file> <module name>
```

## Install as dev dependency
Optionally elm-bare-generator can be added as dev dependency:
```sh
npm add -D elm-bare-generator | yarn add -D elm-bare-generator
```

This allows to create a npm script to generate BARE messages:
```json
{
  "scripts": {
    "generate-messages": "elm-bare-generator <input schema> <output file> <module name>",
  },
}
```

## Caveats
- Anonymous unions are not supported. Extract unions to a named type.
- `int`, `i64` `uint` & `u64` can only hold a maximum of 53 bit.

## Example
This example was created with
```sh
npx elm-bare-generator bare.schema Messages.elm Messages
```

<details>
  <summary>bare.schema</summary>

```
type PublicKey data<128>
type Time string # ISO 8601

enum Sequential {
    RED
    GREEN
    BLUE
}

enum Department {
    ACCOUNTING
    ADMINISTRATION
    CUSTOMER_SERVICE
    DEVELOPMENT

    # Reserved for the CEO
    JSMITH = 99
}

type Customer {
    name: string
    email: string
    address: Address
    orders: []{
        orderId: i64
        quantity: i32
    }
    metadata: map[string]data
}

type Employee {
    name: string
    email: string
    address: Address
    department: Department
    hireDate: Time
    publicKey: optional<PublicKey>
    metadata: map[string]data
}

type TerminatedEmployee void

type Person (Customer | Employee | TerminatedEmployee)

type Address {
    address: [4]string
    city: string
    state: string
    country: string
}

type UnionWithPrimitive (int | void)
```
</details>

<details>
  <summary>Messages.elm</summary>

```elm
module Messages exposing (Address, Customer, Department(..), Employee, Person(..), PublicKey, Sequential(..), Time, UnionWithPrimitive(..), fromAddress, fromCustomer, fromDepartment, fromEmployee, fromPerson, fromPublicKey, fromSequential, fromTime, fromUnionWithPrimitive, toAddress, toCustomer, toDepartment, toEmployee, toPerson, toPublicKey, toSequential, toTime, toUnionWithPrimitive)

import Array exposing (Array)
import Codec.Bare as Codec exposing (Bytes, Codec)
import Dict exposing (Dict)


{-| Convert to message with `fromPublicKey`
  Create from Message with toPublicKey


-}
type alias PublicKey =
    Bytes


{-| Convert to message with `fromTime`
  Create from Message with toTime


-}
type alias Time =
    String


{-| Convert to message with `fromSequential`
  Create from Message with toSequential


-}
type Sequential
    = RED
    | GREEN
    | BLUE


{-| Convert to message with `fromDepartment`
  Create from Message with toDepartment


-}
type Department
    = ACCOUNTING
    | ADMINISTRATION
    | CUSTOMER_SERVICE
    | DEVELOPMENT
    | JSMITH


{-| Convert to message with `fromCustomer`
  Create from Message with toCustomer


-}
type alias Customer =
    { name : String
    , email : String
    , address : Address
    , orders : List { orderId : Int, quantity : Int }
    , metadata : Dict String Bytes
    }


{-| Convert to message with `fromEmployee`
  Create from Message with toEmployee


-}
type alias Employee =
    { name : String
    , email : String
    , address : Address
    , department : Department
    , hireDate : Time
    , publicKey : Maybe PublicKey
    , metadata : Dict String Bytes
    }


{-| Convert to message with `fromPerson`
  Create from Message with toPerson


-}
type Person
    = PersonCustomer Customer
    | PersonEmployee Employee
    | PersonTerminatedEmployee


{-| Convert to message with `fromAddress`
  Create from Message with toAddress


-}
type alias Address =
    { address : List String, city : String, state : String, country : String }


{-| Convert to message with `fromUnionWithPrimitive`
  Create from Message with toUnionWithPrimitive


-}
type UnionWithPrimitive
    = UnionWithPrimitivePrimitive0 Int
    | UnionWithPrimitivePrimitive1


codecOfPublicKey : Codec PublicKey
codecOfPublicKey =
    Codec.dataWithLength 128


codecOfTime : Codec Time
codecOfTime =
    Codec.string


codecOfSequential : Codec Sequential
codecOfSequential =
    Codec.enumWithValues [ ( RED, 0 ), ( GREEN, 1 ), ( BLUE, 2 ) ]


codecOfDepartment : Codec Department
codecOfDepartment =
    Codec.enumWithValues
        [ ( ACCOUNTING, 0 ), ( ADMINISTRATION, 1 ), ( CUSTOMER_SERVICE, 2 ), ( DEVELOPMENT, 3 ), ( JSMITH, 99 ) ]


codecOfCustomer : Codec Customer
codecOfCustomer =
    Codec.struct Customer
        |> Codec.field .name Codec.string
        |> Codec.field .email Codec.string
        |> Codec.field .address codecOfAddress
        |> Codec.field
            .orders
            (Codec.array
                (Codec.struct (\quantity orderId -> { quantity = quantity, orderId = orderId })
                    |> Codec.field .orderId Codec.i64
                    |> Codec.field .quantity Codec.i32
                    |> Codec.buildStruct
                )
            )
        |> Codec.field .metadata (Codec.dict Codec.string Codec.data)
        |> Codec.buildStruct


codecOfEmployee : Codec Employee
codecOfEmployee =
    Codec.struct Employee
        |> Codec.field .name Codec.string
        |> Codec.field .email Codec.string
        |> Codec.field .address codecOfAddress
        |> Codec.field .department codecOfDepartment
        |> Codec.field .hireDate codecOfTime
        |> Codec.field .publicKey (Codec.optional codecOfPublicKey)
        |> Codec.field .metadata (Codec.dict Codec.string Codec.data)
        |> Codec.buildStruct


codecOfPerson : Codec Person
codecOfPerson =
    Codec.taggedUnion
        Codec.variant1 0 PersonCustomer codecOfCustomer
        Codec.variant1 1 PersonEmployee codecOfEmployee
        Codec.variant0 2 PersonTerminatedEmployee
        Codec.buildTaggedUnion


codecOfAddress : Codec Address
codecOfAddress =
    Codec.struct Address
        |> Codec.field .address (Codec.arrayWithLength 4 Codec.string)
        |> Codec.field .city Codec.string
        |> Codec.field .state Codec.string
        |> Codec.field .country Codec.string
        |> Codec.buildStruct


codecOfUnionWithPrimitive : Codec UnionWithPrimitive
codecOfUnionWithPrimitive =
    Codec.taggedUnion
        Codec.variant1 0 UnionWithPrimitivePrimitive0 Codec.int
        Codec.variant0 1 UnionWithPrimitivePrimitive1
        Codec.buildTaggedUnion


toUnionWithPrimitive : Bytes -> Maybe UnionWithPrimitive
toUnionWithPrimitive =
    Codec.decodeValue codecOfUnionWithPrimitive


fromUnionWithPrimitive : UnionWithPrimitive -> Bytes
fromUnionWithPrimitive =
    Codec.encodeToValue codecOfUnionWithPrimitive


toAddress : Bytes -> Maybe Address
toAddress =
    Codec.decodeValue codecOfAddress


fromAddress : Address -> Bytes
fromAddress =
    Codec.encodeToValue codecOfAddress


toPerson : Bytes -> Maybe Person
toPerson =
    Codec.decodeValue codecOfPerson


fromPerson : Person -> Bytes
fromPerson =
    Codec.encodeToValue codecOfPerson


toEmployee : Bytes -> Maybe Employee
toEmployee =
    Codec.decodeValue codecOfEmployee


fromEmployee : Employee -> Bytes
fromEmployee =
    Codec.encodeToValue codecOfEmployee


toCustomer : Bytes -> Maybe Customer
toCustomer =
    Codec.decodeValue codecOfCustomer


fromCustomer : Customer -> Bytes
fromCustomer =
    Codec.encodeToValue codecOfCustomer


toDepartment : Bytes -> Maybe Department
toDepartment =
    Codec.decodeValue codecOfDepartment


fromDepartment : Department -> Bytes
fromDepartment =
    Codec.encodeToValue codecOfDepartment


toSequential : Bytes -> Maybe Sequential
toSequential =
    Codec.decodeValue codecOfSequential


fromSequential : Sequential -> Bytes
fromSequential =
    Codec.encodeToValue codecOfSequential


toTime : Bytes -> Maybe Time
toTime =
    Codec.decodeValue codecOfTime


fromTime : Time -> Bytes
fromTime =
    Codec.encodeToValue codecOfTime


toPublicKey : Bytes -> Maybe PublicKey
toPublicKey =
    Codec.decodeValue codecOfPublicKey


fromPublicKey : PublicKey -> Bytes
fromPublicKey =
    Codec.encodeToValue codecOfPublicKey
```
</details>

## Credits
This project was inspired by [miniBills generator](https://github.com/miniBill/elm-bare-generator). miniBill also created the [elm-bare](https://github.com/miniBill/elm-bare) package for writing BARE message codecs.

Thanks to [DrewDevault](https://drewdevault.com/) for creating the [BARE](https://baremessages.org/) specification.