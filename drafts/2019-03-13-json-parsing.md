---
title: JSON parsing
author: John Ky
---

![JSON railroad diagram for object](https://www.json.org/object.gif)
![JSON railroad diagram for array](https://www.json.org/array.gif)
![JSON railroad diagram for value](https://www.json.org/value.gif)
![JSON railroad diagram for string](https://www.json.org/string.gif)
![JSON railroad diagram for number](https://www.json.org/number.gif)
![RapidJSON state transition diagram](http://rapidjson.org/iterative-parser-states-diagram.png)
![State transition diagram](../images/gen/hw-json/state-transition.svg)

```text
[ES] *           [ES] *           [ES] *           [ES] *
[SS] *           [SJ] "           [SS] *           [SE] \
[VJ] *           [VS] "           [VV] A-Za-z0-9+- [VJ] *
[JJ] *           [JS] "           [JV] A-Za-z0-9+- [JJ] *
```

```text
[ES] *           [ES] "           [ES] A-Za-z0-9+- [ES] \
[SS] *           [SJ] "           [SS] A-Za-z0-9+- [SE] \
[VJ] *           [VS] "           [VV] A-Za-z0-9+- [VJ] \
[JJ] *           [JS] "           [JV] A-Za-z0-9+- [JJ] \
```

![Each transition](../images/gen/hw-json/each-transition.svg)
![Railroad diagram](../images/gen/hw-json/full-railroad.svg)
