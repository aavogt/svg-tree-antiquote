# svg-tree-antiquote

```haskell
-- | Parse svg path commands into svg-tree's 'PathCommand',
-- supporting arithmetic @+ - * / ** ( )@ with haskell-like fixity
--
-- ghci> let x = 7; y = 3 in $(parsePathCommands "v -x**2 h y")
-- [VerticalTo OriginRelative [-49.0],HorizontalTo OriginRelative [3.0]]
parsePathCommands :: String -- ^ https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#path_commands
  -> ExpQ -- ^ ['PathCommand']
```

parse [svg path commands](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/d#path_commands) into [svg-tree](https://hackage-content.haskell.org/package/svg-tree-0.6.2.4/docs/Graphics-Svg-Types.html#t:PathCommand)

used by https://github.com/aavogt/rapids/tree/main/rapids-svg
