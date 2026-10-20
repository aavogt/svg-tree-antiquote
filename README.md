# svg-tree-antiquote

used by https://github.com/aavogt/rapids/tree/main/rapids-svg

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

## Extensions

As an extension, parsePathCommandsSupplement/pathOutline parse and interpret `wWjJ{}`-commands. These represent:

### offset/width

`W a b,c d,e f
L ...` sets the first vertex width to `a` the next to `b` on the left and `c` on the right

### branching

Commands in braces make a branch. A `F` shape is `v 1 { h 1 } v 1 h 1`

### join

TODO. Currently ends are flat (butt) and outside arc or inside
