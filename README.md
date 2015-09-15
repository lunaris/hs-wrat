# wrat

Wrat is a tool for wrapping Haskell type signatures. It's not maintained and
probably a bit out of date with respect to all the type-like things that can be
wrapped in Haskell these days, but it still does a fairly complete job.

## Building

```
% git clone https://github.com/lunaris/hs-wrat
% cd hs-wrat
% stack install
```

## Usage

```
% echo "f :: (Functor f, Monad m) => Maybe (f a) -> (a -> b) -> m (Maybe b)" | wrat
```
