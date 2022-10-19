type EQ<X, Y> = (<T>() => T extends X ? 1 : 2) extends <T>() => T extends Y
  ? 1
  : 2
  ? true
  : false;

type Tuple<X, Y> = [X, Y];

type SwapTuple<T extends [any, any]> = T extends [infer X, infer Y]
  ? [Y, X]
  : never;

// X = Expected
// Y = Given
type EQT<X, Y> = Tuple<Y, EQ<X, Y>>;

type Assert<X extends true | Tuple<any, true>> = X extends true
  ? true
  : X extends Tuple<infer Y, any>
  ? Y
  : never;

type Not<X extends boolean | Tuple<any, boolean>> = X extends true
  ? false
  : X extends false
  ? true
  : X extends Tuple<infer Y, infer _Z>
  ? Y extends true
    ? Tuple<Y, false>
    : Tuple<Y, true>
  : never;

// Natural numbers
type Inc<X extends Natural> = X extends Natural ? `.${X}` : never;
type Dec<X extends Natural> = X extends ""
  ? never
  : X extends `.${infer Y}`
  ? Y
  : "";

type Natural = string;
type Zero = "";
type One = Inc<Zero>;
type Two = Inc<One>;
type Three = Inc<Two>;
type Four = Inc<Three>;
type Five = Inc<Four>;
type Six = Inc<Five>;
type Seven = Inc<Six>;
type Eight = Inc<Seven>;
type Nine = Inc<Eight>;
type Ten = Inc<Nine>;
type Eleven = Inc<Ten>;
type Twelve = Inc<Eleven>;
type Thirteen = Inc<Twelve>;
type FourTeen = Inc<Thirteen>;
type Fifteen = Inc<FourTeen>;

// Show how type assertions work
type _1001 = Assert<EQT<"...", Three>>;
type _1002 = Assert<EQT<Four, Inc<Three>>>;
type _1003 = Assert<EQT<Three, Dec<Four>>>;
type _1004 = Assert<EQT<Five, Inc<Dec<Five>>>>;
type _1005 = Assert<EQT<Five, Dec<Inc<Five>>>>;

// Implement DivMod and Mul in a tail recursive way, tell way and show the difference
type DivMod<
  N extends Natural,
  M extends Natural,
  Q extends Natural = Zero
> = N extends `${M}${infer X}` ? DivMod<X, M, Inc<Q>> : Tuple<Q, N>;

type Mul<
  N extends Natural,
  M extends Natural,
  A extends Natural = Zero
> = M extends Zero ? A : Mul<N, Dec<M>, Add<N, A>>;

type _2001 = Assert<EQT<Tuple<Two, One>, DivMod<Five, Two>>>;
type _2002 = Assert<EQT<Six, Mul<Three, Two>>>;
type _2003 = Assert<EQT<Zero, Mul<Three, Zero>>>;
type _2004 = Assert<EQT<Three, Mul<Three, One>>>;
type _2005 = Assert<EQT<Ten, Mul<One, Ten>>>;
type _2006 = Assert<EQT<Ten, Mul<Ten, One>>>;

// We need Mod, lets write Left and Right
type Left<X extends Tuple<any, any>> = X extends Tuple<infer Y, any>
  ? Y
  : never;
type Right<X extends Tuple<any, any>> = X extends Tuple<any, infer Y>
  ? Y
  : never;

type Div<N extends Natural, M extends Natural> = Left<DivMod<N, M>>;
type Mod<N extends Natural, M extends Natural> = Right<DivMod<N, M>>;

type Add<N extends Natural, M extends Natural> = `${N}${M}`;

type Length<X extends Array<any>, R extends Natural = Zero> = X extends []
  ? R
  : X extends [infer _H, ...infer T]
  ? Length<T, Inc<R>>
  : never;

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

type _3001 = Assert<EQT<One, Mod<Five, Two>>>;

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

type _5001 = Assert<EQT<Mul<Eight, Eight>, FromString<"64">>>;
type _5002 = ToString<FromString<"0">>;
type _5003 = FromString<"0">;
type _5004 = ToString<Zero>;
type _5005 = DivMod<Zero, Ten>;
type _5006 = ToString<Mul<Ten, Ten>>;
type _5007 = Assert<EQT<Mul<Ten, Ten>, FromString<"100">>>;
type _5008 = ToString<FromString<"100">>;

type UniverseTypeWithDimension<
  T extends Natural,
  B,
  R extends Array<B> = []
> = T extends Zero ? R : UniverseTypeWithDimension<Dec<T>, B, [...R, B]>;

type UniverseDimension<U extends UniverseType> = SQRT<Length<U>>;

type DeadCell = " ";
type AliveCell = "#";
type Cell = DeadCell | AliveCell;

type UniverseType = UniverseTypeWithDimension<Mul<Five, Five>, Cell>;

// prettier-ignore
type UniverseS1 = [
  " ", " ", " ", " ", " ",
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

type _5009 = Assert<UniverseS1 extends UniverseType ? true : false>;
type _5010 = Assert<UniverseS2 extends UniverseType ? true : false>;

type LessThan<N extends Natural, M extends Natural> = M extends `${N}${infer Y}`
  ? Y extends ""
    ? false
    : true
  : false;

type _6001 = Assert<UniverseS1 extends UniverseType ? true : false>;
type _6002 = Assert<LessThan<Two, Three>>;
type _6003 = Assert<Not<LessThan<Two, Two>>>;
type _6004 = Assert<Not<LessThan<Three, Two>>>;

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

type CellAt<U extends UniverseType, C extends [Natural, Natural]> = LessThan<
  C[0],
  UniverseDimension<U>
> extends true
  ? LessThan<C[1], UniverseDimension<U>> extends true
    ? U[ToString<CoordinatesToPosition<C, UniverseDimension<U>>>]
    : never
  : never;

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

type _6005 = CellAt<UniverseS1, [Two, Two]>;
type _6006 = Assert<EQT<[Two, One], CoordinatesUp<[Two, Two], Five>>>;
type _6007 = Assert<EQT<[Two, Four], CoordinatesUp<[Two, Zero], Five>>>;
type _6008 = Assert<EQT<[Two, Three], CoordinatesDown<[Two, Two], Five>>>;
type _6009 = Assert<EQT<[Two, Zero], CoordinatesDown<[Two, Four], Five>>>;
type _6010 = Assert<EQT<[One, Two], CoordinatesLeft<[Two, Two], Five>>>;
type _6011 = Assert<EQT<[Four, Two], CoordinatesLeft<[Zero, Two], Five>>>;
type _6012 = Assert<EQT<[Three, Two], CoordinatesRight<[Two, Two], Five>>>;
type _6013 = Assert<EQT<[Zero, Two], CoordinatesRight<[Four, Two], Five>>>;

type NeighboursAt<
  U extends UniverseType,
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

type _7001 = Assert<
  EQT<
    [" ", "#", " ", " ", " ", " ", "#", " "],
    NeighboursAt<UniverseS1, [Two, Two]>
  >
>;

type CountAlive<
  X extends Array<AliveCell | DeadCell>,
  R extends Natural = Zero
> = X extends []
  ? R
  : X extends [infer H, ...infer T]
  ? T extends Array<AliveCell | DeadCell>
    ? H extends AliveCell
      ? CountAlive<T, Inc<R>>
      : CountAlive<T, R>
    : never
  : never;

type _8001 = Assert<EQT<Two, CountAlive<NeighboursAt<UniverseS1, [Two, Two]>>>>;

// if AliveCell and 2 or 3 Alive Neighbours -> stays alive, otherwise dead
// if DeadCell and 3 Alive Neighbours -> then alive, otherwise stays dead
type Evolve<
  C extends AliveCell | DeadCell,
  AliveNeighboursCount extends Natural
> = [C, AliveNeighboursCount] extends [AliveCell, Two | Three]
  ? AliveCell
  : [C, AliveNeighboursCount] extends [DeadCell, Three]
  ? AliveCell
  : DeadCell;

type _9001 = Assert<EQT<DeadCell, Evolve<AliveCell, Zero>>>;
type _9002 = Assert<EQT<DeadCell, Evolve<AliveCell, One>>>;
type _9003 = Assert<EQT<AliveCell, Evolve<AliveCell, Two>>>;
type _9004 = Assert<EQT<AliveCell, Evolve<AliveCell, Three>>>;
type _9005 = Assert<EQT<DeadCell, Evolve<AliveCell, Four>>>;
type _9006 = Assert<EQT<DeadCell, Evolve<AliveCell, Five>>>;
type _9007 = Assert<EQT<DeadCell, Evolve<AliveCell, Six>>>;
type _9008 = Assert<EQT<DeadCell, Evolve<AliveCell, Seven>>>;
type _9009 = Assert<EQT<DeadCell, Evolve<AliveCell, Eight>>>;
type _9010 = Assert<EQT<DeadCell, Evolve<DeadCell, Zero>>>;
type _9011 = Assert<EQT<DeadCell, Evolve<DeadCell, One>>>;
type _9012 = Assert<EQT<DeadCell, Evolve<DeadCell, Two>>>;
type _9013 = Assert<EQT<AliveCell, Evolve<DeadCell, Three>>>;
type _9014 = Assert<EQT<DeadCell, Evolve<DeadCell, Four>>>;
type _9015 = Assert<EQT<DeadCell, Evolve<DeadCell, Five>>>;
type _9016 = Assert<EQT<DeadCell, Evolve<DeadCell, Six>>>;
type _9017 = Assert<EQT<DeadCell, Evolve<DeadCell, Seven>>>;
type _9018 = Assert<EQT<DeadCell, Evolve<DeadCell, Eight>>>;

type EvolveU<
  U extends UniverseType,
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

type _10001 = Assert<EQT<UniverseS2, EvolveU<UniverseS1>>>;
type _10002 = Assert<EQT<UniverseS1, EvolveU<UniverseS2>>>;
