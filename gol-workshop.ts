// Test for equality
type EQ<X, Y> = (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y
  ? 1
  : 2
  ? true
  : false;

// We are gonna need this
type Tuple<X, Y> = [X, Y];

// Equality but we will carry the given value for debugging purpose
type EQT<Expected, Given> = Tuple<Given, EQ<Expected, Given>>;

// Assert for booleans or for the tuple above
type Assert<X extends true | Tuple<any, true>> = X extends true
  ? true
  : X extends Tuple<infer Y, any>
  ? Y
  : never;

// Negation of boolean or for the tuple above
type Not<X extends boolean | Tuple<any, boolean>> = X extends true
  ? false
  : X extends false
  ? true
  : X extends Tuple<infer Y, infer _Z>
  ? Y extends true
    ? Tuple<Y, false>
    : Tuple<Y, true>
  : never;

// STEP-1: Implement SwapTuple, basic `infer`
type _00001 = Assert<EQT<Tuple<1, 2>, SwapTuple<Tuple<2, 1>>>>;
type _00002 = Assert<EQT<Tuple<2, 1>, SwapTuple<Tuple<1, 2>>>>;

// TODO: remove this
type SwapTuple<T extends [any, any]> = T extends [infer X, infer Y]
  ? [Y, X]
  : never;

// Natural numbers representation
type Natural = string;
type Zero = "";
type One = ".";
type Two = "..";
type Three = "...";
type Four = "....";
type Five = ".....";
type Six = "......";
type Seven = ".......";
type Eight = "........";
type Nine = ".........";
type Ten = "..........";

// We can go one like that but it's not practical

// Increment, decrement and addition of a number, no negative numbers
type Inc<X extends Natural> = `.${X}`;
type Dec<X extends Natural> = X extends ""
  ? never
  : X extends `.${infer Y}`
  ? Y
  : "";
type Add<N extends Natural, M extends Natural> = `${N}${M}`;

// STEP-2: Implement Mul, use tail recursion
type _00003 = Assert<EQT<Six, Mul<Three, Two>>>;
type _00004 = Assert<EQT<Nine, Mul<Three, Three>>>;
type _00005 = Assert<EQT<Zero, Mul<Two, Zero>>>;
type _00006 = Assert<EQT<Zero, Mul<Zero, Two>>>;

// TODO: remove this
type Mul<
  N extends Natural,
  M extends Natural,
  A extends Natural = Zero
> = M extends Zero ? A : Mul<N, Dec<M>, Add<N, A>>;

// DivMod: Tuple<Quotient of N/M, Rest of N/M> in the same way as Mul
type _00007 = Assert<EQT<Tuple<Two, One>, DivMod<Five, Two>>>;
type _00008 = Assert<EQT<Six, Mul<Three, Two>>>;
type _00009 = Assert<EQT<Zero, Mul<Three, Zero>>>;
type _00010 = Assert<EQT<Three, Mul<Three, One>>>;
type _00011 = Assert<EQT<Ten, Mul<One, Ten>>>;
type _00012 = Assert<EQT<Ten, Mul<Ten, One>>>;

type DivMod<
  N extends Natural,
  M extends Natural,
  Q extends Natural = Zero
> = N extends `${M}${infer X}` ? DivMod<X, M, Inc<Q>> : Tuple<Q, N>;

// Left and Right of a Tuple
type Left<X extends Tuple<any, any>> = X extends Tuple<infer Y, any>
  ? Y
  : never;
type Right<X extends Tuple<any, any>> = X extends Tuple<any, infer Y>
  ? Y
  : never;

// Length of an array
type Length<X extends Array<any>, R extends Natural = Zero> = X extends []
  ? R
  : X extends [infer _H, ...infer T]
  ? Length<T, Inc<R>>
  : never;

// Check if N is less than M
type LessThan<N extends Natural, M extends Natural> = M extends `${N}${infer Y}`
  ? Y extends ""
    ? false
    : true
  : false;

// Silly way to implement a square root
type SQRT<
  X extends Natural,
  Y extends Natural = Two,
  R extends Natural = Mul<Y, Y>
> = X extends Zero | One
  ? X
  : X extends R
  ? Y
  : LessThan<X, R> extends true
  ? never
  : SQRT<X, Inc<Y>>;

// Easy access to all the Natural numbers
type FromString<S extends String, N extends Natural = Zero> = S extends ""
  ? N
  : S extends `0${infer Y}`
  ? FromString<Y, Mul<N, Ten>>
  : S extends `1${infer Y}`
  ? FromString<Y, Add<One, Mul<N, Ten>>>
  : S extends `2${infer Y}`
  ? FromString<Y, Add<Two, Mul<N, Ten>>>
  : S extends `3${infer Y}`
  ? FromString<Y, Add<Three, Mul<N, Ten>>>
  : S extends `4${infer Y}`
  ? FromString<Y, Add<Four, Mul<N, Ten>>>
  : S extends `5${infer Y}`
  ? FromString<Y, Add<Five, Mul<N, Ten>>>
  : S extends `6${infer Y}`
  ? FromString<Y, Add<Six, Mul<N, Ten>>>
  : S extends `7${infer Y}`
  ? FromString<Y, Add<Seven, Mul<N, Ten>>>
  : S extends `8${infer Y}`
  ? FromString<Y, Add<Eight, Mul<N, Ten>>>
  : S extends `9${infer Y}`
  ? FromString<Y, Add<Nine, Mul<N, Ten>>>
  : never;

// Better way to represent Natural numbers
type ToString<
  N extends Natural,
  S extends string = "",
  M extends Tuple<Natural, Natural> = DivMod<N, Ten>
> = N extends Zero
  ? S extends ""
    ? "0"
    : S
  : Right<M> extends Zero
  ? ToString<Left<M>, `0${S}`>
  : Right<M> extends One
  ? ToString<Left<M>, `1${S}`>
  : Right<M> extends Two
  ? ToString<Left<M>, `2${S}`>
  : Right<M> extends Three
  ? ToString<Left<M>, `3${S}`>
  : Right<M> extends Four
  ? ToString<Left<M>, `4${S}`>
  : Right<M> extends Five
  ? ToString<Left<M>, `5${S}`>
  : Right<M> extends Six
  ? ToString<Left<M>, `6${S}`>
  : Right<M> extends Seven
  ? ToString<Left<M>, `7${S}`>
  : Right<M> extends Eight
  ? ToString<Left<M>, `8${S}`>
  : Right<M> extends Nine
  ? ToString<Left<M>, `9${S}`>
  : never;

type _00013 = Assert<EQT<Mul<Eight, Eight>, FromString<"64">>>;
type _00014 = Assert<EQT<"0", ToString<FromString<"0">>>>;
type _00015 = Assert<EQT<Zero, FromString<"0">>>;
type _00016 = Assert<EQT<"0", ToString<Zero>>>;
type _00018 = Assert<EQT<"100", ToString<Mul<Ten, Ten>>>>;
type _00019 = Assert<EQT<Mul<Ten, Ten>, FromString<"100">>>;
type _00020 = Assert<EQT<"100", ToString<FromString<"100">>>>;

// Game of Life

type DeadCell = " ";
type AliveCell = "#";
type Cell = DeadCell | AliveCell;

type UniverseWithDimension<
  T extends Natural,
  R extends Array<Cell> = []
> = T extends Zero ? R : UniverseWithDimension<Dec<T>, [...R, Cell]>;

type Universe = UniverseWithDimension<Mul<Five, Five>>;

// prettier-ignore
type UniverseS1 = [
  " ", " ", " ", " ", " "
  " ", " ", "#", " ", " ",
  " ", " ", "#", " ", " ",
  " ", " ", "#", " ", " ",
  " ", " ", " ", " ", " "
];

// prettier-ignore
type UniverseS2 = [
  " ", " ", " ", " ", " ",
  " ", " ", " ", " ", " ",
  " ", "#", "#", "#", " ",
  " ", " ", " ", " ", " ",
  " ", " ", " ", " ", " "
];

type _00021 = Assert<UniverseS1 extends Universe ? true : false>;
type _00022 = Assert<UniverseS2 extends Universe ? true : false>;

// Utility to calculate universe dimension
type UniverseDimension<U extends Universe> = SQRT<Length<U>>;

// STEP-3: Implement CellAt<Universe, [X, Y]> : Cell
type _00023 = Assert<EQT<" ", CellAt<UniverseS1, [Zero, Zero]>>>;
type _00024 = Assert<EQT<"#", CellAt<UniverseS1, [Two, Two]>>>;
type _00025 = Assert<EQT<" ", CellAt<UniverseS1, [Four, Four]>>>;
type _00026 = Assert<EQT<never, CellAt<UniverseS1, [Zero, Five]>>>;
type _00027 = Assert<EQT<never, CellAt<UniverseS1, [Zero, Five]>>>;

// TODO: remove this
type CoordinatesToPosition<
  C extends [Natural, Natural],
  D extends Natural
> = LessThan<C[0], D> extends true
  ? LessThan<C[1], D> extends true
    ? Add<Mul<D, C[1]>, C[0]>
    : never
  : never;

type PositionToCoordinates<P extends Natural, D extends Natural> = SwapTuple<
  DivMod<P, D>
>;

type CellAt<
  U extends Universe,
  C extends [Natural, Natural],
  D extends Natural = UniverseDimension<U>
> = LessThan<C[0], D> extends true
  ? LessThan<C[1], D> extends true
    ? U[ToString<CoordinatesToPosition<C, UniverseDimension<U>>>]
    : never
  : never;

// Move coordinates up, down, left and right

type CoordinatesUp<C extends [Natural, Natural], D extends Natural> = EQ<
  C[1],
  Zero
> extends true
  ? [C[0], Dec<D>]
  : [C[0], Dec<C[1]>];

type CoordinatesLeft<C extends [Natural, Natural], D extends Natural> = EQ<
  C[0],
  Zero
> extends true
  ? [Dec<D>, C[1]]
  : [Dec<C[0]>, C[1]];

type CoordinatesDown<C extends [Natural, Natural], D extends Natural> = EQ<
  C[1],
  Dec<D>
> extends true
  ? [C[0], Zero]
  : [C[0], Inc<C[1]>];

type CoordinatesRight<C extends [Natural, Natural], D extends Natural> = EQ<
  C[0],
  Dec<D>
> extends true
  ? [Zero, C[1]]
  : [Inc<C[0]>, C[1]];

type _00028 = Assert<EQT<[Two, One], CoordinatesUp<[Two, Two], Five>>>;
type _00029 = Assert<EQT<[Two, Four], CoordinatesUp<[Two, Zero], Five>>>;
type _00030 = Assert<EQT<[Two, Three], CoordinatesDown<[Two, Two], Five>>>;
type _00031 = Assert<EQT<[Two, Zero], CoordinatesDown<[Two, Four], Five>>>;
type _00032 = Assert<EQT<[One, Two], CoordinatesLeft<[Two, Two], Five>>>;
type _00033 = Assert<EQT<[Four, Two], CoordinatesLeft<[Zero, Two], Five>>>;
type _00034 = Assert<EQT<[Three, Two], CoordinatesRight<[Two, Two], Five>>>;
type _00035 = Assert<EQT<[Zero, Two], CoordinatesRight<[Four, Two], Five>>>;

// STEP-4: Implement NeighboursAt<Universe, [X, Y]> : Array<Cell>
type _00036 = Assert<
  EQT<
    [" ", "#", " ", " ", " ", " ", "#", " "],
    NeighboursAt<UniverseS1, [Two, Two]>
  >
>;

type _00037 = Assert<
  EQT<
    [" ", " ", " ", " ", " ", "#", "#", "#"],
    NeighboursAt<UniverseS2, [Two, One]>
  >
>;

// TODO: remove this
type NeighboursAt<
  U extends Universe,
  C extends [Natural, Natural],
  D extends Natural = UniverseDimension<U>
> = LessThan<C[0], D> extends true
  ? LessThan<C[1], D> extends true
    ? [
        CellAt<U, CoordinatesUp<CoordinatesLeft<C, D>, D>>,
        CellAt<U, CoordinatesUp<C, D>>,
        CellAt<U, CoordinatesUp<CoordinatesRight<C, D>, D>>,
        CellAt<U, CoordinatesLeft<C, D>>,
        CellAt<U, CoordinatesRight<C, D>>,
        CellAt<U, CoordinatesDown<CoordinatesLeft<C, D>, D>>,
        CellAt<U, CoordinatesDown<C, D>>,
        CellAt<U, CoordinatesDown<CoordinatesRight<C, D>, D>>
      ]
    : []
  : [];

// STEP-5: Implement CountAlive<Array<Cell>> : Natural
type _00038 = Assert<
  EQT<Two, CountAlive<NeighboursAt<UniverseS1, [Two, Two]>>>
>;
type _00039 = Assert<
  EQT<Three, CountAlive<NeighboursAt<UniverseS2, [Two, One]>>>
>;

// TODO: remove this
type CountAlive<X extends Array<Cell>, R extends Natural = Zero> = X extends []
  ? R
  : X extends [infer H, ...infer T]
  ? T extends Array<Cell>
    ? H extends AliveCell
      ? CountAlive<T, Inc<R>>
      : CountAlive<T, R>
    : never
  : never;

// STEP-6: Implement Evolve<Cell, Natural> : Cell
type _00101 = Assert<EQT<DeadCell, Evolve<AliveCell, Zero>>>;
type _00102 = Assert<EQT<DeadCell, Evolve<AliveCell, One>>>;
type _00103 = Assert<EQT<AliveCell, Evolve<AliveCell, Two>>>;
type _00104 = Assert<EQT<AliveCell, Evolve<AliveCell, Three>>>;
type _00105 = Assert<EQT<DeadCell, Evolve<AliveCell, Four>>>;
type _00106 = Assert<EQT<DeadCell, Evolve<AliveCell, Five>>>;
type _00107 = Assert<EQT<DeadCell, Evolve<AliveCell, Six>>>;
type _00108 = Assert<EQT<DeadCell, Evolve<AliveCell, Seven>>>;
type _00109 = Assert<EQT<DeadCell, Evolve<AliveCell, Eight>>>;
type _00110 = Assert<EQT<DeadCell, Evolve<DeadCell, Zero>>>;
type _00111 = Assert<EQT<DeadCell, Evolve<DeadCell, One>>>;
type _00112 = Assert<EQT<DeadCell, Evolve<DeadCell, Two>>>;
type _00113 = Assert<EQT<AliveCell, Evolve<DeadCell, Three>>>;
type _00114 = Assert<EQT<DeadCell, Evolve<DeadCell, Four>>>;
type _00115 = Assert<EQT<DeadCell, Evolve<DeadCell, Five>>>;
type _00116 = Assert<EQT<DeadCell, Evolve<DeadCell, Six>>>;
type _00117 = Assert<EQT<DeadCell, Evolve<DeadCell, Seven>>>;
type _00118 = Assert<EQT<DeadCell, Evolve<DeadCell, Eight>>>;

// if AliveCell and 2 or 3 Alive Neighbours -> stays alive, otherwise dead
// if DeadCell and 3 Alive Neighbours -> then alive, otherwise stays dead
// TODO: remove this
type Evolve<
  C extends AliveCell | DeadCell,
  AliveNeighboursCount extends Natural
> = [C, AliveNeighboursCount] extends [AliveCell, Two | Three]
  ? AliveCell
  : [C, AliveNeighboursCount] extends [DeadCell, Three]
  ? AliveCell
  : DeadCell;

// STEP-7: Implement EvolveU<Universe> : Universe
type _10001 = Assert<EQT<UniverseS2, EvolveU<UniverseS1>>>;
type _10002 = Assert<EQT<UniverseS1, EvolveU<UniverseS2>>>;

// TODO: remove this
type EvolveU<
  U extends Universe,
  T extends Array<Cell> = U,
  P extends Natural = Zero,
  R extends Array<Cell> = [],
  C extends [Natural, Natural] = PositionToCoordinates<P, UniverseDimension<U>>,
  N extends Array<any> = NeighboursAt<U, C>
> = T extends []
  ? R
  : T extends [infer UH, ...infer UT]
  ? UH extends Cell
    ? UT extends Array<Cell>
      ? N extends Array<Cell>
        ? EvolveU<U, UT, Inc<P>, [...R, Evolve<UH, CountAlive<N>>]>
        : never
      : never
    : never
  : never;
