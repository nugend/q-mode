; KDB+ mode based on Alvin Shih's mode and
; Simon Garland's vim plugin
;
; Requires rainbow-delimiter and electric-pair modes

(require 'smie)
(require 'rainbow-delimiters)
(require 'smartparens)

(defvar kdb-mode-syntax-table
  (let ((kdb-mode-syntax-table (make-syntax-table)))
    ; suppress emacs recognition of strings so we have control
    (modify-syntax-entry ?_  "w" kdb-mode-syntax-table)
    (modify-syntax-entry ?.  "w" kdb-mode-syntax-table)
    (modify-syntax-entry ?'  "w" kdb-mode-syntax-table)
    (modify-syntax-entry ?$  "." kdb-mode-syntax-table)
    (modify-syntax-entry ?\\ "." kdb-mode-syntax-table)
    (modify-syntax-entry ?\" "_" kdb-mode-syntax-table)

    kdb-mode-syntax-table)
  "Syntax table for kdb-mode")

; build an optimized regex to recognize builtin functions
(defun kdb-builtin-regex-gen ()
  (concat "\\<"
          (regexp-opt
           '(
             ".Q.A" ".Q.Cf" ".Q.IN" ".Q.L" ".Q.M" ".Q.MAP" ".Q.S" ".Q.V" ".Q.a" ".Q.a0" ".Q.a1" ".Q.a2" ".Q.addmonths" ".Q.addr" ".Q.an" ".Q.b6" ".Q.bv" ".Q.chk" ".Q.cn" ".Q.d0" ".Q.dd" ".Q.def" ".Q.dpft" ".Q.dpt" ".Q.dsftg" ".Q.dt" ".Q.en" ".Q.f" ".Q.fc" ".Q.ff" ".Q.fk" ".Q.fl" ".Q.fmt" ".Q.foo" ".Q.fp" ".Q.fs" ".Q.fsn" ".Q.ft" ".Q.fu" ".Q.gc" ".Q.hdpf" ".Q.host" ".Q.id" ".Q.ind" ".Q.j10" ".Q.j12" ".Q.k" ".Q.l" ".Q.n" ".Q.nA" ".Q.nct" ".Q.opt" ".Q.ord" ".Q.p" ".Q.p1" ".Q.p2" ".Q.par" ".Q.pcnt" ".Q.pm" ".Q.ps" ".Q.pt" ".Q.q0" ".Q.qa" ".Q.qb" ".Q.qd" ".Q.qe" ".Q.qm" ".Q.qp" ".Q.qt" ".Q.res" ".Q.s" ".Q.s1" ".Q.s2" ".Q.sw" ".Q.t" ".Q.t0" ".Q.tab" ".Q.tt" ".Q.tx" ".Q.ty" ".Q.ua" ".Q.v" ".Q.view" ".Q.vt" ".Q.w" ".Q.x0" ".Q.x1" ".Q.x10" ".Q.x12" ".Q.x2" ".Q.xy"
             ".h.HOME" ".h.br" ".h.c0" ".h.c1" ".h.cd" ".h.code" ".h.data" ".h.eb" ".h.ec" ".h.ed" ".h.edsn" ".h.es" ".h.ex" ".h.fram" ".h.ha" ".h.hb" ".h.hc" ".h.he" ".h.hn" ".h.hp" ".h.hr" ".h.ht" ".h.hta" ".h.htac" ".h.htc" ".h.html" ".h.http" ".h.hu" ".h.hug" ".h.hy" ".h.iso8601" ".h.jx" ".h.logo" ".h.nbr" ".h.pre" ".h.sa" ".h.sb" ".h.sc" ".h.td" ".h.text" ".h.tx" ".h.ty" ".h.uh" ".h.xd" ".h.xmp" ".h.xs" ".h.xt"
             ".j.J" ".j.c" ".j.d" ".j.j" ".j.k" ".j.q" ".j.s" ".j.v"
             ".o.B0" ".o.C0" ".o.Cols" ".o.Columns" ".o.FG" ".o.Fkey" ".o.Gkey" ".o.Key" ".o.PS" ".o.Special" ".o.Stats" ".o.T" ".o.T0" ".o.TI" ".o.Tables" ".o.Ts" ".o.TypeInfo" ".o.ex" ".o.o" ".o.t"
             ".z.D" ".z.K" ".z.N" ".z.P" ".z.T" ".z.W" ".z.Z" ".z.a" ".z.ac" ".z.b" ".z.bm" ".z.c" ".z.d" ".z.exit" ".z.f" ".z.h" ".z.i" ".z.k" ".z.l" ".z.n" ".z.o" ".z.p" ".z.pc" ".z.pd" ".z.pg" ".z.ph" ".z.pi" ".z.po" ".z.pp" ".z.ps" ".z.pw" ".z.q" ".z.s" ".z.t" ".z.ts" ".z.u" ".z.vs" ".z.w" ".z.wc" ".z.wo" ".z.ws" ".z.x" ".z.z" ".z.zd"
             "abs" "acos" "aj" "aj0" "all" "and" "any" "asc" "asin" "asof" "atan" "attr" "avg" "avgs"
             "bin" "binr"
             "ceiling" "cols" "cor" "cor" "cos" "count" "cov" "cov" "cross" "csv" "cut"
             "delete" "deltas" "desc" "dev" "dev" "differ" "distinct" "div" "do"
             "each" "ej" "enlist" "enlist" "eval" "except" "exec" "exit" "exp"
             "fby" "fills" "first" "fkeys" "flip" "floor" "from"
             "get" "getenv" "group" "gtime"
             "hclose" "hcount" "hdel" "hopen" "hsym"
             "i" "iasc" "idesc" "if" "ij" "in" "insert" "inter" "inv"
             "key" "keys"
             "last" "like" "lj" "load" "log" "lower" "lsq" "ltime" "ltrim"
             "mavg" "max" "maxs" "mcount" "md5" "mdev" "med" "meta" "min" "mins" "mmax" "mmin" "mmu" "mod" "msum"
             "neg" "next" "not" "null"
             "or" "over"
             "parse" "peach" "pj" "plist" "prd" "prds" "prev" "prior"
             "rand" "rank" "ratios" "raze" "read0" "read1" "reciprocal" "reval" "reverse" "rload" "rotate" "rsave" "rtrim"
             "save" "scan" "scov" "sdev" "select" "set" "setenv" "show" "signum" "sin" "sqrt" "ss" "ssr" "string" "sublist" "sum" "sums" "sv" "svar" "system"
             "tables" "tan" "til" "trim" "txf" "type"
             "uj" "ungroup" "union" "update" "upper" "upsert"
             "value" "var" "var" "view" "views" "vs"
             "wavg" "where" "while" "within" "wj" "wj1" "wsum"
             "xasc" "xbar" "xcol" "xcols" "xdesc" "xexp" "xgroup" "xkey" "xlog" "xprev" "xrank"
             ".q.aj" ".q.aj0" ".q.all" ".q.and" ".q.any" ".q.asc" ".q.asof" ".q.attr" ".q.avgs"
             ".q.ceiling" ".q.cols" ".q.count" ".q.cross" ".q.csv" ".q.cut"
             ".q.deltas" ".q.desc" ".q.differ" ".q.distinct" ".q.dsave"
             ".q.each" ".q.ej" ".q.eval" ".q.except"
             ".q.fby" ".q.fills" ".q.first" ".q.fkeys" ".q.flip" ".q.floor"
             ".q.get" ".q.group" ".q.gtime"
             ".q.hclose" ".q.hcount" ".q.hdel" ".q.hopen" ".q.hsym"
             ".q.iasc" ".q.idesc" ".q.ij" ".q.inter" ".q.inv"
             ".q.key" ".q.keys"
             ".q.lj" ".q.ljf"
             ".q.load" ".q.lower" ".q.lsq" ".q.ltime" ".q.ltrim"
             ".q.mavg" ".q.maxs" ".q.mcount" ".q.md5" ".q.mdev" ".q.med" ".q.meta" ".q.mins" ".q.mmax" ".q.mmin" ".q.mmu" ".q.mod" ".q.msum"
             ".q.neg" ".q.next" ".q.not" ".q.null"
             ".q.or" ".q.over"
             ".q.parse" ".q.peach" ".q.pj" ".q.prds" ".q.prev" ".q.prior"
             ".q.rand" ".q.rank" ".q.ratios" ".q.raze" ".q.read0" ".q.read1" ".q.reciprocal" ".q.reverse" ".q.rload" ".q.rotate" ".q.rsave" ".q.rtrim"
             ".q.save" ".q.scan" ".q.set" ".q.show" ".q.signum" ".q.ssr" ".q.string" ".q.sublist" ".q.sums" ".q.sv" ".q.system"
             ".q.tables" ".q.til" ".q.trim" ".q.type"
             ".q.uj" ".q.ungroup" ".q.union" ".q.upper" ".q.upsert"
             ".q.value" ".q.view" ".q.views" ".q.vs"
             ".q.where" ".q.wj" ".q.wj1" ".q.ww"
             ".q.xasc" ".q.xbar" ".q.xcol" ".q.xcols" ".q.xdesc" ".q.xgroup" ".q.xkey" ".q.xlog" ".q.xprev" ".q.xrank"
             ) t) "\\>"))

(defvar kdb-font-lock-keywords
  (list
   ; block comments or trailing comments
   (cons (concat "\\("
                 "^/[ \t]*\n\\(\\([^\\\\].*\\)?\n\\)*\\\\[ \t]*$"
                 "\\)"
                 "\\|"
                 "\\("
                 "^\\\\[ \t]*\n\\(.\\|\n\\)*"
                 "\\)" )
         'font-lock-comment-face)
   '("[ \t]//.*"   . font-lock-comment-face)
   '("^[ \t]*//.*" . font-lock-comment-face)
   '("[ \t]/.*"    . font-lock-comment-face)
   '("^[ \t]*/.*"  . font-lock-comment-face)
   ; strings but allowing embedded \"
   ; also need to match \\ to avoid problems with strings like "\\\\"
   '("\"\\(\\\\[\"\\\\]\\|[^\"]\\)*\"" . font-lock-string-face)

   (cons (kdb-builtin-regex-gen) 'font-lock-builtin-face)

   ;error literals
   '("\\(^\\|[ \t]\\|[^a-zA-Z0-9:,&|~$?@.<>=*+-]\\)'[a-zA-Z][a-zA-Z0-9]*" . font-lock-string-face)

   ; symbol constants
   '("`[:a-zA-Z0-9_][:a-zA-Z0-9_]*" . font-lock-negation-char-face)

   ; variable names
   '("\\<[.a-zA-Z][.a-zA-Z0-9_]*" . font-lock-variable-name-face)

   ; dates and times
   '("[0-9]+D[012][0-9]\\(:[012345][0-9]\\(:[012345][0-9]\\(\\.[0-9]\\{0,9\\}\\)?\\)?\\)?" . font-lock-constant-face)
   '("[0-9]\\{4\\}\\.[01][0-9]\\.[0123][0-9]D[012][0-9]\\(:[012345][0-9]\\(:[012345][0-9]\\(\\.[0-9]\\{0,9\\}\\)?\\)?\\)?" . font-lock-constant-face)
   '("[0-9]\\{4\\}\\.[01][0-9]\\.[0123][0-9]T[012][0-9]\\(:[012345][0-9]\\(:[012345][0-9]\\(\\.[0-9]\\{0,3\\}\\)?\\)?\\)?" . font-lock-constant-face)
   '("[0-9]\\{4\\}\\.[01][0-9]\\.[0123][0-9]" . font-lock-constant-face)
   '("[0-9]\\{4\\}\\.[01][0-9]m" . font-lock-constant-face)
   '("[012][0-9]:[012345][0-9]\\(:[012345][0-9]\\(\\.[0-9]\\{0,3\\}\\)?\\)?" . font-lock-constant-face)

   ; special I/O and IPC functions
   '("[0-2]:" . font-lock-warning-face)

   ; nulls and infinities
   '("-?0W[defghijmnptuvz]?" . font-lock-constant-face)
   '("0N[defghijmnptuvz]?"   . font-lock-constant-face)
   '("0n"   . font-lock-constant-face)
   '("-?0w" . font-lock-constant-face)

   ; bits
   '("[01]+b" . font-lock-constant-face)

   ; floats and reals
   '("-?[0-9]+\\.[0-9]*\\([eE]-?[0-9][0-9]*\\)?[ef]?" . font-lock-constant-face)
   '("-?[0-9]*\\.[0-9]+\\([eE]-?[0-9][0-9]*\\)?[ef]?" . font-lock-constant-face)

   ; bytes
   '("0x[0-9a-fA-F]+" . font-lock-constant-face)

   ; ints, shorts, longs
   '("-?[0-9]+[hj]?" . font-lock-constant-face)

   ; GUIDs
   '("[0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{12\\}")

   ; Operators
   '("[;:,&|~`!$?.@<>=*+-]" . font-lock-function-name-face)

   ; Adverbs
   '("\\(['\\/]\\|/:\\|\\\\:\\|':\\)" . font-lock-function-name-face)

   ; Delimiter Pairs
   '("[](){}[]" . font-lock-type-face)

   ))

(define-derived-mode kdb-mode prog-mode "KDB+/Q Mode"
  "Major Mode for editing KDB+/Q files"

  :syntax-table kdb-mode-syntax-table

  ; Need to fix SMIE so it has knowledge about comments and indentation is a little better
  ;(smie-setup nil #'ignore)

  (set (make-local-variable 'font-lock-defaults) '(kdb-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)

  (add-hook 'kdb-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'kdb-mode-hook 'smartparens-mode)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  )

;;;####autoload
(add-to-list 'auto-mode-alist '("\\.q\\'" . kdb-mode))

;;;####autoload
(add-to-list 'auto-mode-alist '("\\.k\\'" . kdb-mode))
