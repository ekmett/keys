next [????.??.??]
-----------------
* Add `Keyed`, `Indexable`, `Lookup`, `Adjustable`, `FoldableWithKey`, and
  `TraversableWithKey` instances for `Control.Applicative.Const` and
  `Data.Functor.Constant.Constant`.

3.12.2 [2019.05.02]
-------------------
* Use more efficient implementations of `lookup`, `adjust`, `foldMapWithKey`,
  and `traverseWithKey` if building against `containers-0.5.8` or later.

3.12.1 [2018.07.03]
-------------------
* Allow building with `containers-0.6`.
* Avoid the use of deprecated functions from `containers`.

3.12 [2018.01.28]
-----------------
* Add instances for data types in `GHC.Generics`. Change the existing instances
  for `Data.Functor.Sum` to be consistent with those for `(GHC.Generics.:+:)`.
* Add instances for `Proxy` and `Tagged`.
* Add instances for `ZipList`.
* Add `MINIMAL` sets for `Zip` and `FoldableWithKey`.
* Allow `free-5`.

3.11
----
* Support for `comonad` 5
* Support for GHC 8
* Support for `transformers` 0.5

3.10.2
------
* Support for `semigroupoids` 5

3.10.1
------
* Support for `transformers` 0.4

3.10
----
* Updated to use `free`, `semigroupoids`, `comonad` version 4.0

3.0.4
-----
* Updated array dependency
* Added proper upper bounds to other dependencies

3.0.3
-----
* This package is now `Trustworthy`

3.0.2
-----
* Removed upper bounds on my other dependencies
* Directory layout change
* Added support files
* Travis build notification to IRC
