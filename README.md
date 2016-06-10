# foldm

The api exported for `FoldM`s here is the same as that exported for `Fold`s by `Control.Foldl`. Stock folds like `sum`, `mconcat` etc. are defined in terms of `FoldM`. This was originally done for benchmarking - and indeed it seems there is no particular advantage in using pure folds, even in completely pure environments. 

It may be that the module is slightly simpler to use, since it doesn't involve the constant shifting of gears between `Fold` and `FoldM`. Many programs should carry over without change, requiring perhaps a move from `purely` to `impurely`.