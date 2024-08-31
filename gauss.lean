def div_row_at (row : List Float) (i : Nat) : List Float :=
  match row[i]? with
  | some pivotVal => if pivotVal != 0 then row.map (fun x => x / pivotVal) else row
  | none => row

def add_rows (xs ys : List Float) : List Float :=
  match xs, ys with
  | [], _ => []
  | _, [] => []
  | x :: xs', y :: ys' => (x + y) :: add_rows xs' ys'

def mul_row (a : List Float) (factor : Float) : List Float :=
  a.map (fun x => x * factor)

def matrix_to_solutions (m : List (List Float)) : String :=
  m.map (fun row => row.toArray.back?.getD 0)
    |>.enum.map (fun (i, x) => s!"x{i + 1} = {x}")
    |> String.intercalate "\n"

partial def gauss_elim (mat : Array (List Float)) : List (List Float) :=
  let rec elim (m : Array (List Float)) (col : Nat) : List (List Float) :=
    if h : col < m.size then
      let pivotRow := m[col]'h
      let m' := m.mapIdx fun idx row =>
        if idx = col then div_row_at row col
        else
          let factor := row[col]!
          add_rows row (mul_row (div_row_at pivotRow col) (-factor))
      elim m' (col + 1)
    else
      m.toList
  elim mat 0

def main : IO Unit := do
  let arr := #[[4, -2, 1, 3], [1, 3, -2, -5], [-3, -4, 5, 10]]
  gauss_elim arr |> matrix_to_solutions |> IO.println
