# Unreal 源代码阅读笔记

## 整体结构

```[]
Engine
    --(1..n)->
    GameInstance
        --(1..1)->
        WorldContext
            --(1..n)-->
            World
                --(1..n)-->
                Level
                    --(1..n)-->
                    Actor
```