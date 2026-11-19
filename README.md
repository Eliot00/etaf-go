# etaf-go

在 [ETAF](https://github.com/emacs-taf/etaf) 上实现的围棋对弈、SGF 打谱与死活题应用。

对弈是本地双人（同一块棋盘轮流落子）；没有 AI，不依赖 KataGo/OpenSpiel。

## 功能

- 本地双人对弈：轮流落子，内置提子、简单劫、禁着点；`s` 停一手，`R` 认输，
  `u` 撤销，双方连续停一手后自动按数子法给出结果，可保存成 SGF。
- SGF (FF[4]) 打谱：主谱、变化、摆子、注释、SGF 标记、手数、提子数。
- 死活题练习：按 SGF 的下一手判题，答错不落子，答对后自动走对手应对；支持提示和重来。
- 自由研究：从当前局面落子，内置提子、简单劫、禁着点判断；支持停一手。
- 编辑模式：摆黑/白子、擦除、加三角/方块/圆圈/叉/标签、切换行棋方，直接写回 SGF。
- 新建棋谱：从空棋盘摆题，保存成自己的死活题或棋谱，不再依赖外部编辑器。
- 形势判断：按中国数子法标出双方地盘并给出目差，可随时开关。
- 图形端用 SVG 画棋子/棋盘/坐标/地盘，终端端用 box-drawing 字符和底色回退。
- 图形端按实际字体度量自动选择**尽可能大**的接近正方形的 cell；优先保证棋盘完整显示，窗口较矮时棋盘下方的控制区会滚动，窗口不够宽时侧栏移到棋盘下方。`etaf-go-cell-lines` 可强制 cell 行数（`auto` 或 1–8），值越大棋盘越大，窗口放不下时会滚动。
- 亮色/暗色棋盘，9/13/19 路及 2–25 路棋盘。
- 走法列表可点击跳转，注释面板随当前节点更新。

## 依赖

- Emacs 29.1+
- `etaf` 0.3.1+
- `etaf-ui` 0.3.0+

`etaf` 和 `etaf-ui` 目前从 etaf 的包仓库安装：

```elisp
(add-to-list 'package-archives '("etaf" . "https://etaf.top/packages/") t)
(package-refresh-contents)
(package-install 'etaf)
(package-install 'etaf-ui)
```

## 安装

### package-vc

```elisp
(package-vc-install
 '(etaf-go
   :url "https://github.com/Eliot00/etaf-go"))
```

### use-package :vc

需要带 `:vc` 支持的 use-package（当前 Emacs 31 自带的 2.4.5 已支持）：

```elisp
(use-package etaf-go
  :vc (:url "https://github.com/Eliot00/etaf-go"
       :rev :newest)
  :commands (etaf-go etaf-go-open etaf-go-open-file etaf-go-play
             etaf-go-new etaf-go-problems etaf-go-close))
```

入口和所有模块都在仓库根目录，`package-vc` 会把它们一起编译并生成 autoload，
不需要 `:lisp-dir`。本包不设置任何全局按键，需要绑定请在 `:bind` 里自己加。

### 本地开发

包入口和源码都在仓库根目录。开发时直接把仓库根目录加到 `load-path`，不要走安装：

```elisp
(use-package etaf-go
  :load-path "/absolute/path/to/etaf-go"
  :commands (etaf-go etaf-go-open etaf-go-open-file etaf-go-play
             etaf-go-new etaf-go-problems etaf-go-close))
```

注意 `:load-path` 要写**绝对路径**（`~` 不会被 `require` 展开）。

## 使用

```
M-x etaf-go            ; 主界面：对弈 / 打开棋谱 / 死活题 / 新建棋谱
M-x etaf-go-open       ; 同上，打开主界面
M-x etaf-go-play       ; 直接开一局本地双人对弈（默认 19 路）
M-x etaf-go-open-file  ; 直接打开 SGF；C-u 前缀进入死活练习
M-x etaf-go-new        ; 新建空棋谱（默认 19 路），直接进入编辑模式
M-x etaf-go-problems   ; 从 etaf-go-problem-directory 选死活题
```

默认目录：

- 棋谱：`~/.emacs.d/go`
- 死活题：`~/.emacs.d/go/problems`

可以通过 `etaf-go-directory`、`etaf-go-problem-directory` 修改。

## 模式与按键

- `对弈`：点击棋盘轮流落子；`s` 停一手，`R` 认输，`u` 撤销，`r` 重新开始。
  双方连续停一手后结束，自动显示数子结果。
- `打谱`：`n/p` 前后翻页，`g/G` 开头/末手，`v` 选择变化。
- `死活`：点击棋盘作答；`n` 提示下一手；`r` 重来；答对后对手的应手会自动走上。
- `研究`：点击棋盘自由落子；`u` 撤销；`s` 停一手。
- `编辑`：`e` 进入；选工具后点击棋盘修改当前节点，再点 `编辑` 按钮或按 `m` 回到打谱。
  `P` 切换行棋方，`w` 保存。

其他键：`m` 在打谱/死活/研究之间循环，`t` 显示手数，`S` 形势判断，`T` 换主题，
`c` 编辑注释，`x` 删除当前分支，`N` 新建（对弈中是开新局），`o` 打开别的棋谱，
`F1` 帮助，`q` 关闭。

### 做一个自己的死活题

1. `M-x etaf-go-new` 新建 9/13/19 路空盘，自动进入编辑模式。
2. 选 `黑子`/`白子` 摆出题目，`擦除` 改错，`标签`/`三角` 等标注要点。
3. `P` 设置轮到谁走；`c` 写题目说明。
4. `w` 保存为 `.sgf`，之后用 `M-x etaf-go-open-file` 打开，`C-u` 或 `m` 进死活模式练习。

编辑写入当前节点的 `AB`/`AW`/`AE`、`TR`/`SQ`/`CR`/`MA`、`LB` 和 `PL` 属性；
含落子的节点不允许摆子，按 `g` 回到开局再编辑。

坐标列不包含 `I`，与常见围棋记谱一致。

## 开发与测试

```sh
make test     # ERT：规则、SGF、SVG 单元格、真实 UI 事件链路
make check    # 字节编译 + 测试
make smoke    # 批处理里挂载主界面
```

这台机器的 `emacs` 不在 PATH 时，Makefile 会回退到
`/Applications/Emacs.app/Contents/MacOS/Emacs`；也可以显式指定：

```sh
make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs check
```

干净的开发会话：

```sh
/Applications/Emacs.app/Contents/MacOS/Emacs -Q -l dev/init-dev.el
```

`dev/init-dev.el` 会设置 `load-prefer-newer` 和示例目录，不安装快捷键：

- `M-x etaf-go` 打开主界面
- `M-x etaf-go-dev-reload` 重新加载并回到主界面

ETAF Component 有重复定义保护。改 `etaf-go-view.el` 后不要直接 `C-M-x`；
先关闭 app，再用 `etaf-go-dev-reload`（它内部用 `etaf-component-redefine-run`），
或者直接重启 Emacs。

## 边界

- 只实现简单劫，不实现超级劫/循环劫。
- 形势判断是数子法的静态估计：只把「四周全是同色」的空区算作地盘，
  公活/劫争/死活未定处按中性处理，不是终局判定。
- 编辑只修改当前节点；含落子的节点要先按 `g` 回到开局再摆子。
- 对弈目前是本地双人；没有 AI，KataGo/OpenSpiel 接入是后续独立工作。
- 对弈结束后的数子结果只显示、不写回 SGF 的 `RE`；保存的是完整着法树。
- 暂不做题库进度、搜索和批量管理。

## License

GPL-3.0-or-later，见 `LICENSE`。
