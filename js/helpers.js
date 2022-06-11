export function data_byteString(bsStr) {
  // {{{
  return PlutusData.new_bytes(Buffer.from(bsStr, 'hex'));
  // }}}
}

export function data_integer(intStr) {
  // {{{
  return PlutusData.new_integer(BigInt.from_str(intStr));
  // }}}
}

export function data_list(plutusDataVals) {
  // {{{
  let theList = PlutusList.new();
  plutusDataVals.forEach(pd => theList.add(pd));
  return PlutusData.new_list(theList);
  // }}}
}

export function data_map(plutusDataPairs) {
  // {{{
  let theMap = PlutusMap.new();
  plutusDataPairs.forEach(
    pd => theMap.insert(pd[0], pd[1])
  );
  return PlutusData.new_map(theList);
  // }}}
}

export function data_constr(indexStr, plutusDataVals) {
  // {{{
  let theList = PlutusList.new();
  plutusDataVals.forEach(pd =>
    theList.add(pd)
  );
  return PlutusData.new_constr_plutus_data(
    ConstrPlutusData.new(
      BigNum.from_str(indexStr),
      theList
    )
  );
  // }}}
}
