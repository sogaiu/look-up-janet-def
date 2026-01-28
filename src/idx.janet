#! /usr/bin/env janet

(comment import ./index-c :prefix "")
(comment import ./index :prefix "")
(defn idx/get-first-lines-and-offsets!
  [src-str filtered key-name]
  (var cur-line 1)
  (var pos 0)
  # XXX: will \n work on all platforms?
  (def eol "\n")
  (def eol-len (length eol))
  (each entry filtered
    (def {key-name name} entry)
    (def [_ attrs _] name)
    (def line-no (get attrs :bl))
    (def line-diff
      (- line-no cur-line))
    (repeat line-diff
      (set pos
           (+ (string/find eol src-str pos)
              eol-len)))
    (put entry
         :offset pos)
    (def end-pos
      (+ (string/find eol src-str pos)
         eol-len))
    (put entry
         :first-line
         (string/slice src-str
                       pos (- end-pos eol-len)))
    (set pos end-pos)
    (set cur-line (inc line-no)))
  #
  filtered)

(defn idx/get-all-pieces
  [src-str captures]
  (def results @[])
  (var cur-line 1)
  (var pos 0)
  # XXX: will \n work on all platforms?
  (def eol "\n")
  (def eol-len (length eol))
  (each entry captures
    # XXX: have position here, but ignoring
    (def [line-no _ _ id] entry)
    # XXX: hack to capture all ids in an array
    (array/push (dyn :all-ids) id)
    (def line-diff
      (- line-no cur-line))
    (repeat line-diff
      (set pos
           (+ (string/find eol src-str pos)
              eol-len)))
    (def end-pos
      (+ (string/find eol src-str pos)
         eol-len))
    (array/push results
                [(string/slice src-str
                               pos (- end-pos eol-len))
                 id
                 (string line-no)
                 (string pos)])
    (set pos end-pos)
    (set cur-line (inc line-no)))
  #
  results)

(defn idx/index-file!
  [src path tags-fn out-buf]
  (def form-feed
    (string/from-bytes 0x0C))
  (def start-of-heading
    (string/from-bytes 0x01))
  (def delete
    (string/from-bytes 0x7F))
  # XXX
  #(def start (os/clock))
  (def tags
    (tags-fn src))
  #(printf "tags-fn: %p" (- (os/clock) start))
  (when (not (empty? tags))
    # XXX: will this eol always be "\n" on every platform?
    (def eol "\n")
    (var tags-byte-count 0)
    (+= tags-byte-count
        (reduce (fn [acc [first-line id line-no file-offset]]
                  (+ acc
                     (length first-line)
                     # delete
                     1
                     (length id)
                     # start of heading
                     1
                     (length line-no)
                     # comma
                     1
                     # XXX: char- vs byte- offset issue
                     #(length file-offset)
                     # XXX: eol
                     1))
                0
                tags))
    #
    (buffer/push out-buf
                 form-feed eol)
    (buffer/push out-buf
                 path ","
                 # total size of what follows -- assumes eol is one byte?
                 (string tags-byte-count)
                 eol)
    (each [first-line id line-no file-offset] tags
      (buffer/push out-buf
                   # first line of text without line-ending
                   first-line
                   delete
                   # identifier name
                   id
                   start-of-heading
                   # line
                   line-no
                   ","
                   # XXX: char- vs byte- offset issue
                   # offset from start of file
                   #file-offset
                   eol)))
  #
  out-buf)


# capture part of these things, but recognize them so they
# can be navigated "over"
#
# #define JANET_DEFINE_MATH2OP(name, fop, signature, doc)\
# JANET_CORE_FN(janet_##name, signature, doc) {\
#     janet_fixarity(argc, 2); \
#     double lhs = janet_getnumber(argv, 0); \
#     double rhs = janet_getnumber(argv, 1); \
#     return janet_wrap_number(fop(lhs, rhs)); \
# }
#
# #define OPMETHOD(T, type, name, oper) \
# static Janet cfun_it_##type##_##name(int32_t argc, Janet *argv) { \
#     janet_arity(argc, 2, -1); \
#     T *box = janet_abstract(&janet_##type##_type, sizeof(T)); \
#     *box = janet_unwrap_##type(argv[0]); \
#     for (int32_t i = 1; i < argc; i++) \
#         /* This avoids undefined behavior. See above for why. */ \
#         *box = (T) ((uint64_t) (*box)) oper ((uint64_t) janet_unwrap_##type(argv[i])); \
#     return janet_wrap_abstract(box); \
# } \

(def ic/col-one
  ~{:main (some (choice :comment
                        :macro-define
                        :non-macro-match
                        :not-match))
    :non-macro-match (cmt (sequence (look -1 "\n")
                                    (not :s)
                                    (not "#")
                                    (not "}")
                                    (not :label)
                                    (line) (column) (position)
                                    (capture (to "\n"))
                                    "\n")
                          ,|@{:bl $0
                              :bc $1
                              :bp $2
                              :text $3})
    :label (sequence :id ":")
    :id (some (choice :a :d "_"))
    :comment (choice (sequence "//"
                               (any (if-not (set "\r\n") 1)))
                     (sequence "/*"
                               (any (if-not `*/` 1))
                               "*/"))
    :macro-define (choice (cmt (sequence (line) (column) (position)
                                         (capture (sequence "#define" (to "\n")))
                                         "\n")
                               ,|@{:bl $0
                                   :bc $1
                                   :bp $2
                                   :text $3})
                          (cmt (sequence (line) (column) (position)
                                         (capture (sequence "#define" (to `\`)))
                                         `\` "\n"
                                         (some (sequence (thru `\`) "\n"))
                                         # sometimes this is not how it ends
                                         (opt "\n}"))
                               ,|@{:bl $0
                                   :bc $1
                                   :bp $2
                                   :text $3}))
    :not-match 1})

# see comment form below for concrete examples
(defn ic/find-id-for-td-en-st-line
  [line position src]
  (def rev
    (string/reverse line))
  # remember to think backwards as the matching is happening from
  # what was originally the "right" side of the string
  (def g
    '(choice (sequence (choice ";)"
                               ",")
                       (thru "(")
                       (choice (sequence ")"
                                         (capture (choice (to "*")
                                                          (to "("))))
                               (capture (to (set " *")))))
             (sequence ";"
                       (capture (to (set " *"))))
             (sequence "{"
                       :s+
                       (choice (sequence "="
                                         :s+
                                         "]"
                                         (thru "[")
                                         (capture (to (set " *"))))
                               (sequence ")"
                                         (thru "(")
                                         (capture (to (set " *"))))
                               # mune is enum reversed
                               # tcurts is struct reversed
                               (sequence (not "mune")
                                         (not "tcurts")
                                         (capture (to " ")))
                               (constant :reparse)))))
  (def m
    (peg/match g rev))
  # this peg is not for the reversed string
  (def g2
    ~{:main (sequence (some (sequence :id :s+))
                      :curlies
                      :s+ (capture :id) ";")
      :id (some (choice :a :d "_"))
      # XXX: might work with nested because of the source's formatting
      :curlies (sequence "{"
                         (to "\n}")
                         "\n}")})
  #
  (when-let [capture (first m)]
    (if (= :reparse capture)
      (when-let [m2 (peg/match g2 src position)
                 capture-2 (first m2)]
        capture-2)
      # XXX
      #:reparse
      (string/reverse capture))))

(comment

  (ic/find-id-for-td-en-st-line
    "typedef double (win64_variant_f_ffff)(double, double, double, double);"
    nil nil)
  # =>
  "win64_variant_f_ffff"

  (ic/find-id-for-td-en-st-line
    (string "typedef sysv64_sseint_return "
            "janet_sysv64_variant_4(uint64_t a, uint64_t b, uint64_t c, "
            "uint64_t d, uint64_t e, uint64_t f,")
    nil nil)
  # =>
  "janet_sysv64_variant_4"

  (ic/find-id-for-td-en-st-line
    "typedef struct _stat jstat_t;"
    nil nil)
  # =>
  "jstat_t"

  (ic/find-id-for-td-en-st-line
    "enum JanetInstructionType janet_instructions[JOP_INSTRUCTION_COUNT] = {"
    nil nil)
  # =>
  "janet_instructions"

  (ic/find-id-for-td-en-st-line
    "enum JanetParserStatus janet_parser_status(JanetParser *parser) {"
    nil nil)
  # =>
  "janet_parser_status"

  (ic/find-id-for-td-en-st-line
    "enum JanetMemoryType {" nil nil)
  # =>
  "JanetMemoryType"

  (ic/find-id-for-td-en-st-line
    "struct BigNat {" nil nil)
  # =>
  "BigNat"

  (ic/find-id-for-td-en-st-line
    "typedef struct JanetEnvRef {" nil nil)
  # =>
  "JanetEnvRef"

  (ic/find-id-for-td-en-st-line
    "typedef void (*Special)(Builder *b, int32_t argc, const Janet *argv);"
    nil nil)
  # =>
  "Special"

  (def src
    ``
    enum {
        LB_REAL = 200,
        LB_NIL, /* 201 */
        LB_FALSE, /* 202 */
        LB_TRUE,  /* 203 */
        LB_FIBER, /* 204 */
        LB_INTEGER, /* 205 */
        LB_STRING, /* 206 */
        LB_SYMBOL, /* 207 */
        LB_KEYWORD, /* 208 */
        LB_ARRAY, /* 209 */
        LB_TUPLE, /* 210 */
        LB_TABLE, /* 211 */
        LB_TABLE_PROTO, /* 212 */
        LB_STRUCT, /* 213 */
        LB_BUFFER, /* 214 */
        LB_FUNCTION, /* 215 */
        LB_REGISTRY, /* 216 */
        LB_ABSTRACT, /* 217 */
        LB_REFERENCE, /* 218 */
        LB_FUNCENV_REF, /* 219 */
        LB_FUNCDEF_REF, /* 220 */
        LB_UNSAFE_CFUNCTION, /* 221 */
        LB_UNSAFE_POINTER, /* 222 */
        LB_STRUCT_PROTO, /* 223 */
    #ifdef JANET_EV
        LB_THREADED_ABSTRACT, /* 224 */
        LB_POINTER_BUFFER, /* 224 */
    #endif
    } LeadBytes;
    ``)

  (ic/find-id-for-td-en-st-line
    "enum {" 0 src)
  # =>
  "LeadBytes"

  (def src
    ``
    typedef enum {
        JANET_ASYNC_WRITEMODE_WRITE,
        JANET_ASYNC_WRITEMODE_SEND,
        JANET_ASYNC_WRITEMODE_SENDTO
    } JanetWriteMode;
    ``)

  (ic/find-id-for-td-en-st-line
    "typedef enum {" 0 src)
  # =>
  "JanetWriteMode"

  (def src
    ``
    typedef struct {
        JanetEVGenericMessage msg;
        JanetThreadedCallback cb;
        JanetThreadedSubroutine subr;
        JanetHandle write_pipe;
    } JanetEVThreadInit;
    ``)

  (ic/find-id-for-td-en-st-line
    "typedef struct {" 0 src)
  # =>
  "JanetEVThreadInit"

  (def src
    ``
    typedef struct {
        const uint8_t *text_start;
        const uint8_t *text_end;
        const uint32_t *bytecode;
        const Janet *constants;
        JanetArray *captures;
        JanetBuffer *scratch;
        JanetBuffer *tags;
        JanetArray *tagged_captures;
        const Janet *extrav;
        int32_t *linemap;
        int32_t extrac;
        int32_t depth;
        int32_t linemaplen;
        int32_t has_backref;
        enum {
            PEG_MODE_NORMAL,
            PEG_MODE_ACCUMULATE
        } mode;
    } PegState;
    ``)

  (ic/find-id-for-td-en-st-line
    "typedef struct {" 0 src)
  # =>
  "PegState"

  )

(defn ic/find-id-for-rest
  [line]
  (def rev
    (string/reverse line))
  (def has-equals
    (string/find "=" rev))
  (def start
    (inc (or has-equals
             -1)))
  (defn dprintf
    [fmt & args]
    (when (os/getenv "VERBOSE")
      (printf fmt ;args)))
  # XXX
  (dprintf "start: %d" start)
  (dprintf "rev from start: %s" (string/slice rev start))
  (def g
    ~(sequence
       :s*
       # XXX: not the most general
       (any (choice (sequence "/*" (thru `*/`))
                    (sequence (thru "//"))))
       :s*
       (choice
         (sequence ","
                   (thru "(")
                   (cmt (capture (to (set " *")))
                        ,|(do
                            (dprintf ",")
                            $)))
         (sequence "]"
                   (thru "[")
                   (cmt (capture (to (set " *")))
                        ,|(do
                            (dprintf "]")
                            $)))
         (sequence ";"
                   (choice (sequence "]"
                                     (thru "[")
                                     (cmt (capture (to (set " *")))
                                          ,|(do
                                              (dprintf ";]")
                                              $)))
                           (sequence ")"
                                     (constant :declaration))
                           (cmt (capture (to (set " *")))
                                ,|(do
                                    (dprintf "; default")
                                    $))))
         (sequence "{" :s+
                   (choice
                     (sequence ")"
                               (thru "(")
                               (choice (sequence ")"
                                                 (cmt (capture (to (set "*(")))
                                                      ,|(do
                                                          (dprintf "{) up")
                                                          $)))
                                       (cmt (capture (to (choice (set " *")
                                                                 -1)))
                                            ,|(do
                                                (dprintf "{) down")
                                                $))))
                     (cmt (capture (to (set " *")))
                          ,|(do
                              (dprintf "{ default")
                              $))))
         (sequence "("
                   (cmt (capture (to (set " *")))
                        ,|(do
                            (dprintf "(")
                            $)))
         (cmt (capture (to (set " *")))
              ,|(do
                  (dprintf "default")
                  $)))))
  #
  (def m
    (peg/match g rev start))
  # XXX
  (dprintf "%p" m)
  #
  (when-let [capture (first m)]
    (if (= :declaration capture)
      :declaration
      (string/reverse capture))))

(comment

  #(os/setenv "VERBOSE" "1")

  (ic/find-id-for-rest
    "const char *const janet_signal_names[14] = {")
  # =>
  "janet_signal_names"

  (ic/find-id-for-rest
    "static char error_clib_buf[256];")
  # =>
  "error_clib_buf"

  (ic/find-id-for-rest
    "static int cfun_io_gc(void *p, size_t len);")
  # =>
  :declaration

  (ic/find-id-for-rest
    "JANET_THREAD_LOCAL JanetVM janet_vm;")
  # =>
  "janet_vm"

  (ic/find-id-for-rest
    "double (janet_unwrap_number)(Janet x) {")
  # =>
  "janet_unwrap_number"

  (ic/find-id-for-rest
    "const Janet *(janet_unwrap_tuple)(Janet x) {")
  # =>
  "janet_unwrap_tuple"

  (ic/find-id-for-rest
    "os_proc_wait_impl(JanetProc *proc) {")
  # =>
  "os_proc_wait_impl"

  (ic/find-id-for-rest
    "const void *janet_strbinsearch(")
  # =>
  "janet_strbinsearch"

  (ic/find-id-for-rest
    "static const JanetAbstractType janet_struct_type = {")
  # =>
  "janet_struct_type"

  (ic/find-id-for-rest
    "static void janetc_movenear(JanetCompiler *c,")
  # =>
  "janetc_movenear"

  )

(defn ic/find-id-for-macro-define
  [line]
  (def g
    ~(sequence "#define" :s+
               (capture (to (set " (")))))
  (def m
    (peg/match g line))

  (first m))

(comment

  (ic/find-id-for-macro-define
    "#define A ((*pc >> 8)  & 0xFF)")
  # =>
  "A"

  (ic/find-id-for-macro-define
    (string "#define janet_v_free(v)         "
            "(((v) != NULL) ? (janet_sfree(janet_v__raw(v)), 0) : 0)"))
  # =>
  "janet_v_free"

  (ic/find-id-for-macro-define
    "#define vm_throw(e) do { vm_commit(); janet_panic(e); } while (0)")
  # =>
  "vm_throw"

  (ic/find-id-for-macro-define
    "#define JANET_EMIT_H")
  # =>
  nil

  )

(defn ic/separate-lines
  [samples]
  (def scan-from-right @[])
  # typedef, enum, struct
  (def td-en-st @[])
  (def macro-defines @[])
  (def unmatched @[])
  (loop [i :in samples]
    (def s (get i :text))
    (when (not (or (string/has-prefix? "extern " s)
                   (peg/match '(sequence (some (range "AZ" "09" "__"))
                                         "(")
                              s)))
      (cond
        (or (string/has-prefix? "typedef " s)
            (string/has-prefix? "enum " s)
            (string/has-prefix? "struct " s))
        (array/push td-en-st i)
        #
        (string/has-prefix? "#define" s)
        (array/push macro-defines i)
        #
        (not (peg/match '(some (choice :a :d "_"))
                        (string/reverse s)))
        (array/push scan-from-right i)
        # for introspection
        (array/push unmatched i))))
  #
  [scan-from-right td-en-st macro-defines unmatched])

(comment

  (def dir
    (string (os/getenv "HOME")
            "/src/janet/src/core"))

  (def samples
    (seq [path :in (os/dir dir)
          :let [full-path (string dir "/" path)
                src (slurp full-path)]
          item :in (peg/match ic/col-one src)]
      # XXX: src or path?
      (put item :src src)))

  (def [scan-from-right td-en-st macro-defines unmatched]
    (ic/separate-lines samples))

  (var cnt 0)

  (each i (sort-by |(get $ :text) td-en-st)
    (def s
      (get i :text))
    (def position
      (get i :bp))
    (def src
      (get i :src))
    (def result
      (ic/find-id-for-td-en-st-line s position src))
    (when (string? result)
      (++ cnt))
    (printf "%p" result))

  (each i (sort-by |(get $ :text) scan-from-right)
    (def s
      (get i :text))
    (def result (ic/find-id-for-rest s))
    (when (string? result)
      (++ cnt))
    (printf "%p" result))

  (each i (sort-by |(get $ :text) macro-defines)
    (def s
      (get i :text))
    (def result
      (ic/find-id-for-macro-define s))
    (when (string? result)
      (++ cnt))
    (printf "%p" result))

  # 1293, 1629
  cnt

  )

########################################################################

(defn ic/find-c-tags
  [src]

  (def results @[])

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/core/math.c")))

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/core/ev.c")))

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/core/vector.h")))

  (def caps
    (peg/match ic/col-one src))

  (def [scan-from-right td-en-st macro-defines unmatched]
    (ic/separate-lines caps))

  # XXX: what about duplicates?
  (each item scan-from-right
    (def line
      (get item :text))
    (def id-maybe
      (ic/find-id-for-rest line))
    (when (string? id-maybe)
      (def line-no
        (get item :bl))
      (def pos
        (get item :bp))
      (array/push results
                  [line
                   id-maybe
                   (string line-no)
                   (string pos)])))
  # XXX: what about duplicates?
  (each item td-en-st
    (def line
      (get item :text))
    (def pos
      (get item :bp))
    (def id-maybe
      (ic/find-id-for-td-en-st-line line pos src))
    (when (string? id-maybe)
      (def line-no
        (get item :bl))
      (array/push results
                  [line
                   id-maybe
                   (string line-no)
                   (string pos)])))
  # XXX: what about duplicates?
  (each item macro-defines
    (def line
      (get item :text))
    (def pos
      (get item :bp))
    (def id-maybe
      (ic/find-id-for-macro-define line))
    (when (string? id-maybe)
      (def line-no
        (get item :bl))
      (array/push results
                  [line
                   id-maybe
                   (string line-no)
                   (string pos)])))
  # enum constants
  (each item td-en-st
    (def line
      (get item :text))
    (def pos
      (get item :bp))
    (when (or (and (string/has-prefix? "enum " line)
                   (string/has-suffix? "{" line))
              (and (string/has-prefix? "typedef enum " line)
                   (string/has-suffix? "{" line)))
      (def m
        (peg/match
          ~{:main (sequence (opt (sequence "typedef" :s+))
                            "enum" :s+
                            (opt (sequence :id :s+))
                            "{" "\n"
                            (some (cmt (sequence (not "}")
                                                 (line) (column) (position)
                                                 (capture (to "\n")) "\n")
                                       ,|@{:bl $0
                                           :bc $1
                                           :bp $2
                                           :text $3})))
            :id (some (choice :a :d "_"))}
          src pos))
      (when m
        (each item m
          (def line-no
            (get item :bl))
          (def pos
            (get item :bp))
          (def line
            (get item :text))
          (def trimmed
            (string/trim line))
          (def id
            (string/slice trimmed
                          0 (or (string/find "," trimmed)
                                -1)))
          (array/push results
                      [line
                       id
                       (string line-no)
                       (string pos)])))))
  #
  results)

########################################################################

(defn ic/index-c!
  [src path out-buf]
  (idx/index-file! src path ic/find-c-tags out-buf))


(comment import ./index-j2c :prefix "")
(comment import ./index :prefix "")


########################################################################

(defn ij2c/find-math-c-tags
  [src]

  '(def src
    (slurp
      (string (os/getenv "HOME") "/src/janet/src/core/math.c")))

  # JANET_DEFINE_MATHOP(acos, "Returns the arccosine of x.")
  # ...
  # JANET_DEFINE_NAMED_MATHOP("log-gamma", lgamma, "Returns log-gamma(x).")
  # ...
  # JANET_DEFINE_MATH2OP(pow, pow, "(math/pow a x)", "Returns a to the power of x.")
  # ...

  (def query-peg
    ~{:main (some (choice :match
                          :non-match))
      :id (some (choice :a :d "_"))
      :match
      (sequence "\n"
                (choice (sequence "JANET_DEFINE_NAMED_MATHOP" "("
                                  (cmt (sequence `"`
                                                 (line) (column) (position)
                                                 (capture (to `"`)))
                                       ,|[$0 $1 $2 (string "math/" $3)]))
                        #
                        (sequence "JANET_DEFINE_MATH2OP" "("
                                  :id "," :s+
                                  :id "," :s+
                                  (cmt (sequence `"(`
                                                 (line) (column) (position)
                                                 (capture (to (set " )"))))
                                       ,|[$0 $1 $2 $3]))
                        #
                        (sequence "JANET_DEFINE_MATHOP" "("
                                  (cmt (sequence (line) (column) (position)
                                                 (capture :id))
                                       ,|[$0 $1 $2 (string "math/" $3)]))))
      :non-match 1})

  (def caps
    (peg/match query-peg src))

  (idx/get-all-pieces src caps))

(defn ij2c/find-specials-c-tags
  [src]

  '(def src
    (slurp
      (string (os/getenv "HOME") "/src/janet/src/core/specials.c")))

  # static JanetSlot janetc_quote(JanetFopts opts, int32_t argn, const Janet *argv) {
  # ...
  # static JanetSlot janetc_varset(JanetFopts opts, int32_t argn, const Janet *argv) {
  # ...

  (def query-peg
    ~{:main (some (choice :match
                          :non-match))
      :id (some (choice :a :d "_"))
      :match (sequence "\n" "static JanetSlot" :s+
                       (cmt (sequence (line) (column) (position)
                                      (capture :id))
                            ,(fn [l c p id]
                               (def prefix "janetc_")
                               (when (string/has-prefix? prefix id)
                                 (def short-name
                                   (string/slice id (length prefix)))
                                 (def real-name
                                   (if (= "varset" short-name)
                                     "set"
                                     short-name))
                                 [l c p real-name]))))
      :non-match 1})

  (def caps
    (peg/match query-peg src))

  (idx/get-all-pieces src caps))

(defn ij2c/find-corelib-c-tags
  [src]

  '(def src
    (slurp
      (string (os/getenv "HOME") "/src/janet/src/core/corelib.c")))

  # janet_quick_asm(env, JANET_FUN_APPLY | JANET_FUNCDEF_FLAG_VARARG,
  #                 "apply", 1, 1, INT32_MAX, 6, apply_asm, sizeof(apply_asm),
  # ...
  # janet_quick_asm(env, JANET_FUN_MODULO,
  #                 "mod", 2, 2, 2, 2, modulo_asm, sizeof(modulo_asm),
  #                 JDOC("(mod dividend divisor)\n\n"
  #                      "Returns the modulo of dividend / divisor."));
  # ...
  # templatize_varop(env, JANET_FUN_MULTIPLY, "*", 1, 1, JOP_MULTIPLY,
  #                  JDOC("(* & xs)\n\n"
  #                       "Returns the product ... returns 1."));
  # ...
  # templatize_comparator(env, JANET_FUN_GT, ">", 0, JOP_GREATER_THAN,
  #                       JDOC("(> & xs)\n\n"
  #                       "Check if xs is in ... Returns a boolean."));
  # ...
  # janet_def(env, "janet/version", janet_cstringv(JANET_VERSION),
  #           JDOC("The version number of the running janet program."));

  (def query-peg
    ~{:main (some (choice :match
                          :non-match))
      :id (some (choice :a :d "_"))
      :match
      (sequence
        ";\n"
        # * the /* ... */ part is for +, >, janet/version, root-env
        (opt (sequence :s+
                       "/*"
                       (some (if-not "*/" 1))
                       "*/"))
        :s+
        (choice (sequence (choice "janet_quick_asm"
                                  "templatize_varop"
                                  "templatize_comparator")
                          "("
                          :id
                          "," :s+
                          :id (opt (sequence :s+ "|" :s+ :id)) # opt for apply
                          "," :s+
                          (cmt (sequence `"`
                                         (line) (column) (position)
                                         (capture (to `"`))
                                         `"`)
                               ,|[$0 $1 $2 $3]))
                (sequence "janet_def"
                          "("
                          :id "," :s+
                          (cmt (sequence `"`
                                         (line) (column) (position)
                                         (capture (to `"`))
                                         `"`)
                               ,|[$0 $1 $2 $3]))))
      :non-match 1})

  (def caps
    (peg/match query-peg src))

  (idx/get-all-pieces src caps))

# JANET_CORE_DEF
# * io.c
# * math.c
(defn ij2c/find-janet-core-def-tags
  [src]

  '(def src
    (slurp
      (string (os/getenv "HOME") "/src/janet/src/core/io.c")))

  '(def src
    (slurp
      (string (os/getenv "HOME") "/src/janet/src/core/math.c")))

  # #ifdef JANET_BOOTSTRAP
  #     JANET_CORE_DEF(env, "math/pi", janet_wrap_number(3.1415926535897931),
  #                    ...);
  # ...
  #
  # note that leading whitespace is elided from sample of io.c below
  #
  # int default_flags = JANET_FILE_NOT_CLOSEABLE | JANET_FILE_SERIALIZABLE;
  # /* stdout */
  # JANET_CORE_DEF(env, "stdout",
  #                ...);
  # /* stderr */
  # JANET_CORE_DEF(env, "stderr",
  #                ...);

  (def query-peg
    ~{:main (some (choice :match
                          :non-match))
      :id (some (choice :a :d "_"))
      :match (sequence (choice ";" :id)
                       (opt (sequence :s+
                                      "/*"
                                      (some (if-not "*/" 1))
                                      "*/"))
                       :s+
                       "JANET_CORE_DEF"
                       "("
                       :id "," :s+
                       (cmt (sequence `"`
                                      (line) (column) (position)
                                      (capture (to `"`))
                                      `"`)
                            ,|[$0 $1 $2 $3]))
      :non-match 1})

  (def caps
    (peg/match query-peg src))

  (idx/get-all-pieces src caps))

# JANET_CORE_FN
# * many
(defn ij2c/find-janet-core-fn-tags
  [src]

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/core/ffi.c")))

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/core/math.c")))

  # JANET_CORE_FN(cfun_peg_compile,
  #              "(peg/compile peg)", ...)

  (def query-peg
    ~{:main (some (choice :match
                          :non-match))
      :id (some (choice :a :d "_"))
      :match (sequence (choice ";"  # parser/state, etc.
                               ")"  # math/pow, etc.
                               "}"  # ev/acquire-lock, etc.
                               "*/" # module/expand-path, etc.
                               :id) # ffi/signature, etc.
                       (choice (sequence :s+
                                         "/*"
                                         (some (if-not "*/" 1))
                                         "*/"
                                         :s+)
                               :s+)
                       "JANET_CORE_FN" "(" :id "," :s+
                       # e.g. "(file/temp)" or "(peg/compile peg)"
                       (cmt (sequence `"(`
                                      (line) (column) (position)
                                      (capture (to (choice (set ` )`)
                                                           # janet 1.17.0 has
                                                           # an error in doc
                                                           # for
                                                           # fiber/last-value
                                                           `"`))))
                            ,|[$0 $1 $2 $3]))
      :non-match 1})

  (def caps
    (peg/match query-peg src))

  (idx/get-all-pieces src caps))

# const JanetAbstractType janet... = {
# * many
(defn ij2c/find-janet-abstract-type-tags
  [src]

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/core/ev.c")))

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/core/ffi.c")))

  # const JanetAbstractType janet... = {
  #     "core/file",
  #     ...
  # };

  (def query-peg
    ~{:main (some (choice :match
                          :non-match))
      :id (some (choice :a :d "_"))
      :match (sequence "const" :s+
                       "JanetAbstractType" :s+
                       :id :s+ "="
                       :s+ "{" :s+
                       (cmt (sequence `"`
                                      (line) (column) (position)
                                      (capture (some (if-not `"` 1)))
                                      `"`)
                            ,|[$0 $1 $2 $3]))
      :non-match 1})

  (def caps
    (peg/match query-peg src))

  (idx/get-all-pieces src caps))

########################################################################

(defn ij2c/index-math-c!
  [src path out-buf]
  (idx/index-file! src path ij2c/find-math-c-tags out-buf))

(defn ij2c/index-specials-c!
  [src path out-buf]
  (idx/index-file! src path ij2c/find-specials-c-tags out-buf))

(defn ij2c/index-corelib-c!
  [src path out-buf]
  (idx/index-file! src path ij2c/find-corelib-c-tags out-buf))

(defn ij2c/index-janet-core-def-c!
  [src path out-buf]
  (idx/index-file! src path ij2c/find-janet-core-def-tags out-buf))

(defn ij2c/index-generic-c!
  [src path out-buf]
  (try
    (idx/index-file! src path ij2c/find-janet-abstract-type-tags out-buf)
    ([e]
      (eprintf "%s: abstract - %p" path e)))
  (try
    (idx/index-file! src path ij2c/find-janet-core-fn-tags out-buf)
    ([e]
      (eprintf "%s: core-fn - %p" path e))))


(comment import ./index-janet :prefix "")
(comment import ./index :prefix "")

(comment import ./janet-cursor :prefix "")
(comment import ./janet-peg :prefix "")
(comment import ./loc :prefix "")
# bl - begin line
# bc - begin column
# bp - begin position
# el - end line
# ec - end column
# ep - end position
(defn l/make-attrs
  [& args]
  (zipcoll [:bl :bc :bp :el :ec :ep]
           args))

(comment

  (l/make-attrs 1 1 0
              10 20 50)
  # =>
  @{:bc 1 :bl 1 :bp 0 :ec 20 :el 10 :ep 50}

  )

(comment

  (defn opaque-node
    [the-type peg-form]
    ~(cmt (capture (sequence (line) (column) (position)
                             ,peg-form
                             (line) (column) (position)))
          ,|[the-type
             (l/make-attrs ;(tuple/slice $& 0 3)
                         ;(tuple/slice $& (- (- 3) 2) -2))
             (last $&)]))

  (defn delim-node
    [the-type open close]
    ~(cmt
       (capture
         (sequence
           (line) (column) (position)
           ,open
           (any :input)
           (choice ,close
                   (error
                     (replace (sequence (line) (column) (position))
                              ,|(string/format
                                  (string "line: %d column: %d pos: %d "
                                          "missing %s for %s")
                                  $0 $1 $2 close the-type))))
           (line) (column) (position)))
       ,|[the-type
          (l/make-attrs ;(tuple/slice $& 0 3)
                      ;(tuple/slice $& (- (- 3) 2) -2))
          ;(tuple/slice $& 3 (- (- 3 ) 2))]))

  (def t-grammar
    ~{:main (some :input)
      :input (choice :ws :str :dl)
      :ws ,(opaque-node :ws '(set " \n"))
      :str ,(opaque-node :str
                         '(sequence `"`
                                    (any (if-not `"` 1))
                                    `"`))
      :dl ,(delim-node :dl `(` `)`)})

  (peg/match t-grammar `"hi there"`)
  # =>
  '@[(:str @{:bc 1 :bl 1 :bp 0 :ec 11 :el 1 :ep 10} "\"hi there\"")]

  (peg/match t-grammar `("alice" "bob")`)
  # =>
  '@[(:dl @{:bc 1 :bl 1 :bp 0 :ec 16 :el 1 :ep 15}
          (:str @{:bc 2 :bl 1 :bp 1 :ec 9 :el 1 :ep 8} "\"alice\"")
          (:ws @{:bc 9 :bl 1 :bp 8 :ec 10 :el 1 :ep 9} " ")
          (:str @{:bc 10 :bl 1 :bp 9 :ec 15 :el 1 :ep 14} "\"bob\""))]

  )



(defn jp/make-grammar
  [&opt opts]
  #
  (def opaque-node
    (or (get opts :opaque-node)
        (fn [the-type peg-form]
          ~(cmt (capture (sequence (line) (column) (position)
                                   ,peg-form
                                   (line) (column) (position)))
                ,|[the-type
                   (l/make-attrs ;(tuple/slice $& 0 3)
                                 ;(tuple/slice $& (- (- 3) 2) -2))
                   (last $&)]))))
  #
  (def delim-node
    (or (get opts :delim-node)
        (fn [the-type open close]
          ~(cmt
             (capture
               (sequence
                 (line) (column) (position)
                 ,open
                 (any :input)
                 (choice ,close
                         (error
                           (replace (sequence (line) (column) (position))
                                    ,|(string/format
                                        (string "line: %d column: %d pos: %d "
                                                "missing %s for %s")
                                        $0 $1 $2 close the-type))))
                 (line) (column) (position)))
             ,|[the-type
                (l/make-attrs ;(tuple/slice $& 0 3)
                              ;(tuple/slice $& (- (- 3) 2) -2))
                ;(tuple/slice $& 3 (- (- 3 ) 2))]))))
  #
  ~{:main (sequence (line) (column) (position)
                    (some :input)
                    (line) (column) (position))
    #
    :input (choice :ws
                   :cmt
                   :form)
    #
    :ws (choice :ws/horiz
                :ws/eol)
    #
    :ws/horiz ,(opaque-node :ws/horiz
                            '(some (set " \0\f\t\v")))
    #
    :ws/eol ,(opaque-node :ws/eol
                          '(choice "\r\n"
                                   "\r"
                                   "\n"))
    #
    :cmt :cmt/line
    #
    :cmt/line
    ,(opaque-node :cmt/line
                  '(sequence "#"
                             (any (if-not (set "\r\n") 1))))
    #
    :form (choice :str
                  :blob
                  :dl)
    #
    :str (choice :str/dq
                 :str/bt)
    #
    :str/dq
    ,(opaque-node :str/dq
                  '(sequence `"`
                             (any (choice :escape
                                          (if-not `"` 1)))
                             `"`))
    #
    :escape (sequence `\`
                      (choice (set `"'0?\abefnrtvz`)
                              (sequence "x" (2 :h))
                              (sequence "u" (4 :h))
                              (sequence "U" (6 :h))
                              (error (constant "bad escape"))))
    #
    :str/bt
    ,(opaque-node :str/bt
                  ~{:main (drop (sequence :open
                                          (any (if-not :close 1))
                                          :close))
                    :open (capture :delim :n)
                    :delim (some "`")
                    :close (cmt (sequence (not (look -1 "`"))
                                          (backref :n)
                                          (capture (backmatch :n)))
                                ,=)})
    #
    :blob
    ,(opaque-node
       :blob
       '(some (choice (range "09" "AZ" "az" "\x80\xFF")
                      (set "!$%&*+-./:<=>?^_")
                      # XXX: what to do about mutable collections...
                      "@"
                      # XXX: possibly separate...
                      (set "|~';,"))))
    #
    :dl (choice :dl/round
                :dl/square
                :dl/curly)
    #
    :dl/round ,(delim-node :dl/round "(" ")")
    #
    :dl/square ,(delim-node :dl/square "[" "]")
    #
    :dl/curly ,(delim-node :dl/curly "{" "}")})

(comment

  (def grammar (jp/make-grammar))

  (get (peg/match grammar `2`) 3)
  # =>
  '(:blob @{:bc 1 :bl 1 :bp 0
            :ec 2 :el 1 :ep 1}
          "2")

  (get (peg/match grammar `(+ 1 1)`) 3)
  # =>
  '(:dl/round @{:bc 1 :bl 1 :bp 0 :ec 8 :el 1 :ep 7}
              (:blob @{:bc 2 :bl 1 :bp 1 :ec 3 :el 1 :ep 2} "+")
              (:ws/horiz @{:bc 3 :bl 1 :bp 2 :ec 4 :el 1 :ep 3} " ")
              (:blob @{:bc 4 :bl 1 :bp 3 :ec 5 :el 1 :ep 4} "1")
              (:ws/horiz @{:bc 5 :bl 1 :bp 4 :ec 6 :el 1 :ep 5} " ")
              (:blob @{:bc 6 :bl 1 :bp 5 :ec 7 :el 1 :ep 6} "1"))

  (-> (peg/match grammar `@[:a :b :c]`)
      (array/slice 3 (dec (- 3))))
  # =>
  '@[(:blob @{:bc 1 :bl 1 :bp 0 :ec 2 :el 1 :ep 1} "@")
     (:dl/square @{:bc 2 :bl 1 :bp 1 :ec 12 :el 1 :ep 11}
                 (:blob @{:bc 3 :bl 1 :bp 2 :ec 5 :el 1 :ep 4} ":a")
                 (:ws/horiz @{:bc 5 :bl 1 :bp 4 :ec 6 :el 1 :ep 5} " ")
                 (:blob @{:bc 6 :bl 1 :bp 5 :ec 8 :el 1 :ep 7} ":b")
                 (:ws/horiz @{:bc 8 :bl 1 :bp 7 :ec 9 :el 1 :ep 8} " ")
                 (:blob @{:bc 9 :bl 1 :bp 8 :ec 11 :el 1 :ep 10} ":c"))]

  (get (peg/match grammar
                  ``
                  (defn fun
                    [x]
                    (+ x 1))
                  ``)
       3)
  # =>
  '(:dl/round
     @{:bc 1 :bl 1 :bp 0 :ec 11 :el 3 :ep 26}
     (:blob @{:bc 2 :bl 1 :bp 1 :ec 6 :el 1 :ep 5} "defn")
     (:ws/horiz @{:bc 6 :bl 1 :bp 5 :ec 7 :el 1 :ep 6} " ")
     (:blob @{:bc 7 :bl 1 :bp 6 :ec 10 :el 1 :ep 9} "fun")
     (:ws/eol @{:bc 10 :bl 1 :bp 9 :ec 1 :el 2 :ep 10} "\n")
     (:ws/horiz @{:bc 1 :bl 2 :bp 10 :ec 3 :el 2 :ep 12} "  ")
     (:dl/square @{:bc 3 :bl 2 :bp 12 :ec 6 :el 2 :ep 15}
                 (:blob @{:bc 4 :bl 2 :bp 13 :ec 5 :el 2 :ep 14} "x"))
     (:ws/eol @{:bc 6 :bl 2 :bp 15 :ec 1 :el 3 :ep 16} "\n")
     (:ws/horiz @{:bc 1 :bl 3 :bp 16 :ec 3 :el 3 :ep 18} "  ")
     (:dl/round @{:bc 3 :bl 3 :bp 18 :ec 10 :el 3 :ep 25}
                (:blob @{:bc 4 :bl 3 :bp 19 :ec 5 :el 3 :ep 20} "+")
                (:ws/horiz @{:bc 5 :bl 3 :bp 20 :ec 6 :el 3 :ep 21} " ")
                (:blob @{:bc 6 :bl 3 :bp 21 :ec 7 :el 3 :ep 22} "x")
                (:ws/horiz @{:bc 7 :bl 3 :bp 22 :ec 8 :el 3 :ep 23} " ")
                (:blob @{:bc 8 :bl 3 :bp 23 :ec 9 :el 3 :ep 24} "1")))

  (get (peg/match grammar
                  ``
                  (print # nice comment
                    "hello")
                  ``)
       3)
  # =>
  '(:dl/round
     @{:bc 1 :bl 1 :bp 0 :ec 11 :el 2 :ep 32}
     (:blob @{:bc 2 :bl 1 :bp 1 :ec 7 :el 1 :ep 6} "print")
     (:ws/horiz @{:bc 7 :bl 1 :bp 6 :ec 8 :el 1 :ep 7} " ")
     (:cmt/line @{:bc 8 :bl 1 :bp 7 :ec 22 :el 1 :ep 21} "# nice comment")
     (:ws/eol @{:bc 22 :bl 1 :bp 21 :ec 1 :el 2 :ep 22} "\n")
     (:ws/horiz @{:bc 1 :bl 2 :bp 22 :ec 3 :el 2 :ep 24} "  ")
     (:str/dq @{:bc 3 :bl 2 :bp 24 :ec 10 :el 2 :ep 31} "\"hello\""))

  )


# to make generic cursor functions available
#(import ./cursor :prefix "" :export true)
(comment import ./cursor :prefix "")
(comment import ./loc :prefix "")


(defn c/init-infra
  [make-grammar]
  (var counter 0)

  (defn issue-id
    []
    (++ counter))

  (def id->node @{})

  (def loc->id @{})

  (defn reset
    []
    (set counter 0)
    (table/clear id->node)
    (table/clear loc->id))

  (defn opaque-node
    [node-type peg-form]
    ~(cmt (capture (sequence (line) (column) (position)
                             ,peg-form
                             (line) (column) (position)))
          ,|(let [id (issue-id)
                  attrs (l/make-attrs ;(tuple/slice $& 0 -2))
                  _ (put loc->id (freeze attrs) id)
                  node [node-type
                        (put attrs :id id)
                        (last $&)]]
              (put id->node id node)
              node)))

  (defn delim-node
    [node-type open-delim close-delim]
    ~(cmt
       (capture
         (sequence
           (line) (column) (position)
           ,open-delim
           (any :input)
           (choice ,close-delim
                   (error
                     (replace (sequence (line) (column) (position))
                              ,|(string/format
                                  (string "line: %p column: %p pos: %p "
                                          "missing %p for %p")
                                  $0 $1 $2 close-delim node-type))))
           (line) (column) (position)))
       ,|(let [id (issue-id)
               attrs (l/make-attrs ;(tuple/slice $& 0 3)
                                   ;(tuple/slice $& (- (- 3) 2) -2))
               _ (put loc->id (freeze attrs) id)
               # add the index position and parent id for each child
               [_ children]
               (reduce (fn add-idx-and-pid
                         [[counter kids] child]
                         # XXX
                         #(d/deprintf "counter: %n" counter)
                         #(d/deprintf "kids: %n" kids)
                         #(d/deprintf "child: %n" child)
                         (def [_ attrs _] child)
                         # XXX
                         #(d/deprintf "type: %n" (type attrs))
                         (unless (= :table (type attrs))
                           (eprintf "child: %n" child)
                           (eprintf "$&: %n" $&))
                         (put attrs :idx counter)
                         (put attrs :pid id)
                         [(inc counter)
                          (array/push kids child)])
                       # index and to-be-filled-with-children
                       [0 @[]]
                       # children before
                       (tuple/slice $& 3 (- (- 3) 2)))
               node [node-type
                     (put attrs :id id)
                     ;children]]
           (put id->node id node)
           node)))

  (def loc-grammar
    (make-grammar {:opaque-node opaque-node
                   :delim-node delim-node}))

  #
  (defn par
    [src &opt start single]
    (default start 0)
    (def top-id 0)
    (def loc-top-level-ast
      (let [ltla (table ;(kvs loc-grammar))]
        (put ltla
             :main ~(sequence (line) (column) (position)
                              :input
                              (line) (column) (position)))
        (table/to-struct ltla)))
    #
    (def top-node
      (if single
        (if-let [[bl bc bp tree el ec ep]
                 (peg/match loc-top-level-ast src start)]
          @[:code
            (put (l/make-attrs bl bc bp el ec ep)
                 :id top-id)
            tree]
          @[:code])
        (if-let [captures (peg/match loc-grammar src start)]
          (let [[bl bc bp] (array/slice captures 0 3)
                [el ec ep] (array/slice captures (dec -3))
                [_ trees] (reduce (fn [[counter kids] child]
                                    (def [_ attrs _] child)
                                    (put attrs :idx counter)
                                    (put attrs :pid top-id)
                                    [(inc counter)
                                     (array/push kids child)])
                                  [0 @[]]
                                  (array/slice captures 3 (dec -3)))]
            (array/insert trees 0
                          :code (put (l/make-attrs bl bc bp el ec ep)
                                     :id top-id)))
          @[:code])))
    #
    (put id->node top-id top-node)
    #
    top-node)
  #
  {:grammar loc-grammar
   :node-table id->node
   :loc-table loc->id
   :issuer issue-id
   :reset reset
   :parse par})

(defn c/make-cursor
  [node-table &opt node]
  (default node (get node-table 0))
  {:node node
   :table node-table})

(defn c/right
  [{:node n :table n-tbl}]
  (def [_ attrs _] n)
  (when-let [pid (get attrs :pid)
             idx (get attrs :idx)
             [_ _ & rest] (get n-tbl pid)]
    (when (tuple? rest)
      (when-let [next-sibling (get rest (inc idx))]
        {:node next-sibling
         :table n-tbl}))))

(defn c/up
  [{:node n :table n-tbl}]
  (def [_ attrs _] n)
  (when-let [pid (get attrs :pid)]
    {:node (get n-tbl pid)
     :table n-tbl}))

(defn c/down
  [{:node n :table n-tbl}]
  (def [_ _ & rest] n)
  (when (tuple? rest)
    (when-let [first-elt (first rest)]
      (when (tuple? first-elt)
        {:node first-elt
         :table n-tbl}))))

(defn c/df-next
  [crs]
  #
  (defn helper
    [a-crs]
    (if-let [up-cand (c/up a-crs)]
      (or (c/right up-cand)
          (helper up-cand))
      :back-at-top))
  # XXX: this part might be off a bit
  (or (c/down crs)
      (c/right crs)
      (helper crs)))

(defn c/rightmost
  [{:node node :table node-table}]
  (def [_ attrs _] node)
  (when-let [pid (get attrs :pid)
             [_ _ & rest] (get node-table pid)]
    (when (tuple? rest) # should not fail
      (when-let [last-sibling (last rest)]
        {:node last-sibling
         :table node-table}))))

(defn c/left
  [{:node n :table n-tbl}]
  (def [_ attrs _] n)
  (when-let [pid (get attrs :pid)
             idx (get attrs :idx)
             [_ _ & rest] (get n-tbl pid)]
    (when (tuple? rest)
      (when-let [prev-sibling (get rest (dec idx))]
        {:node prev-sibling
         :table n-tbl}))))

(defn c/df-prev
  [crs]
  #
  (defn helper
    [a-crs]
    (if-let [down-cand (c/down a-crs)]
      (helper (c/rightmost down-cand))
      a-crs))
  #
  (if-let [left-cand (c/left crs)]
    (helper left-cand)
    (c/up crs)))



(def jc/init-infra c/init-infra)

(def jc/make-cursor c/make-cursor)

(def jc/right c/right)

(def jc/up c/up)

(def jc/down c/down)

(def jc/df-next c/df-next)

(def jc/rightmost c/rightmost)

(def jc/left c/left)

(def jc/df-prev c/df-prev)

(defn jc/make-infra
  []
  (jc/init-infra jp/make-grammar))

(comment

  (def {:grammar loc-grammar
        :issuer issue-id
        :node-table id->node
        :loc-table loc->id
        :reset reset}
    (jc/make-infra))

  (reset)

  (get (peg/match loc-grammar "1") 3)
  # =>
  '(:blob @{:bc 1 :bl 1 :bp 0
            :ec 2 :el 1 :ep 1
            :id 1}
          "1")

  (get (peg/match loc-grammar "[1]") 3)
  # =>
  '(:dl/square @{:bc 1 :bl 1 :bp 0
                 :ec 4 :el 1 :ep 3
                 :id 3}
               (:blob @{:bc 2 :bl 1 :bp 1
                        :ec 3 :el 1 :ep 2
                        :id 2 :idx 0 :pid 3}
                      "1"))

  id->node
  # =>
  '@{1
     (:blob @{:bc 1 :bl 1 :bp 0
              :ec 2 :el 1 :ep 1
              :id 1}
            "1")
     2
     (:blob @{:bc 2 :bl 1 :bp 1
              :ec 3 :el 1 :ep 2
              :id 2 :idx 0 :pid 3}
            "1")
     3
     (:dl/square @{:bc 1 :bl 1 :bp 0
                   :ec 4 :el 1 :ep 3
                   :id 3}
                 (:blob @{:bc 2 :bl 1 :bp 1
                          :ec 3 :el 1 :ep 2
                          :id 2 :idx 0 :pid 3}
                        "1"))}

  loc->id
  # =>
  '@{{:bc 2 :bl 1 :bp 1 :ec 3 :el 1 :ep 2} 2
     {:bc 1 :bl 1 :bp 0 :ec 4 :el 1 :ep 3} 3
     {:bc 1 :bl 1 :bp 0 :ec 2 :el 1 :ep 1} 1}

  (array/slice (peg/match loc-grammar "|[2 3]")
               3 (dec (- 3)))
  # =>
  '@[(:blob @{:bc 1 :bl 1 :bp 0
              :ec 2 :el 1 :ep 1 :id 4} "|")
     (:dl/square @{:bc 2 :bl 1 :bp 1 :ec 7 :el 1 :ep 6 :id 8}
                 (:blob @{:bc 3 :bl 1 :bp 2
                          :ec 4 :el 1 :ep 3 :id 5 :idx 0 :pid 8} "2")
                 (:ws/horiz @{:bc 4 :bl 1 :bp 3
                              :ec 5 :el 1 :ep 4 :id 6 :idx 1 :pid 8} " ")
                 (:blob @{:bc 5 :bl 1 :bp 4
                          :ec 6 :el 1 :ep 5 :id 7 :idx 2 :pid 8} "3"))]

  )




(comment import ./janet-query :prefix "")
(comment import ./loc :prefix "")

(comment import ./janet-peg :prefix "")

(comment import ./debug :prefix "")
(defn d/deprintf
  [fmt & args]
  (when (dyn :ij-debug)
    (eprintf fmt ;args)))

(defn d/deprint
  [msg]
  (when (dyn :ij-debug)
    (eprint msg)))


# options should be a dictionary with things such as:
#
# * blank delimiter safe character info
#   * single delim for both sides
#   * left delim diff from right delim
#   * open-delim / close-delim fixed chars?
#   * raw-string like delimiting?
# * string escape character info
# * blob character info
# * possibly other things eventually
#   * line comment info
#   * multi-line comment info
#   * whitespace info
#   * raw string info
#
# XXX: could "lint" the options, e.g. conflict in blob char with
#      blank delims
(defn jq/make-infra
  [&opt opts]

  (def safe-delim
    (if-let [bd (get opts :safe-delim)]
      bd
      `\`))

  (def loc->node @{})

  (def n-safe-delims @[])

  (defn opaque-node
    [the-type peg-form]
    ~(cmt (capture (sequence (line) (column) (position)
                             ,peg-form
                             (line) (column) (position)))
          # XXX: ;(tuple/slice $& 0 -2) might work here
          ,|(let [attrs (l/make-attrs ;(tuple/slice $& 0 3)
                                      ;(tuple/slice $& (- (- 3) 2) -2))
                  node [the-type attrs (last $&)]]
              (put loc->node (freeze attrs) node)
              node)))

  (defn delim-node
    [the-type open close]
    ~(cmt
       (capture
         (sequence
           (line) (column) (position)
           ,open
           (any :input)
           (choice ,close
                   (error
                     (replace (sequence (line) (column) (position))
                              ,|(string/format
                                  (string "line: %d column: %d pos: %d "
                                          "missing %s for %s")
                                  $0 $1 $2 close the-type))))
           (line) (column) (position)))
       ,|(let [attrs (l/make-attrs ;(tuple/slice $& 0 3)
                                   ;(tuple/slice $& (- (- 3) 2) -2))
               node [the-type attrs ;(tuple/slice $& 3 (- (- 3 ) 2))]]
           (put loc->node (freeze attrs) node)
           node)))

  (def lang-grammar
    (jp/make-grammar {:opaque-node opaque-node
                      :delim-node delim-node}))

  (def query-grammar
    (-> (struct/to-table lang-grammar)
        (put
          :form (let [old-form (get lang-grammar :form)]
                  (tuple 'choice
                         :blank
                         ;(tuple/slice old-form 1))))
        (put :... '(any :input))
        (put
          :blank
          ~(cmt (capture
                  (sequence (line) (column) (position)
                            (capture :blank-internal)
                            (line) (column) (position)))
                ,|(let [attrs
                        (l/make-attrs ;(tuple/slice $& 0 3)
                                      ;(tuple/slice $& (- (- 3) 2) -2))
                        n (array/pop n-safe-delims)
                        [value] (slice $& 3 (- (- 3) 2))]
                    # XXX
                    (d/deprintf "$&: %n" $&)
                    (d/deprintf "attrs: %n" attrs)
                    (d/deprintf "value: %n" value)
                    (d/deprintf "n: %d" n)
                    # discard the surrounding blank delimiters
                    [:blank attrs (string/slice value n (dec (- n)))])))
        (put
          :blank-internal
          ~{:main (drop (sequence :open
                                  (any (if-not :close 1))
                                  :close))
            :open (capture :delim :n)
            # use "safe" delimiters, e.g. $, if possible?
            :delim (some ,safe-delim)
            :close (cmt (sequence (not (look -1 ,safe-delim))
                                  (backref :n)
                                  (capture (backmatch :n)))
                        ,(fn [left right]
                           (when (= left right)
                             # hack to pass back number of safe-delims
                             (array/push n-safe-delims
                                         (length left))
                             true)))})))
  #
  (defn parse-query
    [src &opt start single]
    (default start 0)
    (def top-level-ast
      (let [tla (table ;(kvs query-grammar))]
        (put tla
             :main ~(sequence (line) (column) (position)
                              :input
                              (line) (column) (position)))
        (table/to-struct tla)))
    #
    (def top-node
      (if single
        (if-let [[bl bc bp tree el ec ep]
                 (peg/match top-level-ast src start)]
          @[:code (l/make-attrs bl bc bp el ec ep) tree]
          @[:code])
        (if-let [captures (peg/match query-grammar src start)]
          (let [[bl bc bp] (slice captures 0 3)
                [el ec ep] (slice captures (dec -3))
                trees (array/slice captures 3 (dec -3))]
            (array/insert trees 0
                          :code (l/make-attrs bl bc bp el ec ep)))
          @[:code])))
    #
    top-node)
  # must start with ::
  (defn blank-sym-name?
    [cand]
    (peg/match
      '(sequence "::"
                 (some (choice (range "09" "AZ" "az" "\x80\xFF")
                               (set "!$%&*+-./:<?=>@^_")))
                 -1)
      cand))

  (defn parse-blank-data
    [blank-data]
    # special name meaning to match (but don't capture) 0 or more :input
    (when (= ":..." blank-data)
      (break [~(drop ,(get query-grammar :...))]))
    #
    (def parse-results (parse-all blank-data))
    (def n-results (length parse-results))
    (assert (pos? n-results)
            (string/format "Failed to parse: %n" blank-data))
    (def [head neck] parse-results)
    #
    (case n-results
      1
      (cond
        # only ~ something and ' something
        (and (tuple? head)
             (or (= 'quasiquote (first head))
                 (= 'quote (first head))))
        # drop is here to ensure no captures happen
        [~(drop ,(eval head))]
        #
        (or (number? head) (string? head))
        # numbers and strings cannot capture so no drop needed
        [head]
        # keyword from grammar means to match
        (and (keyword? head)
             (get lang-grammar head))
        [~(drop ,head)]
        #
        (errorf "Unrecognized first item: %n in blank-data: %n"
                head blank-data))
      2
      (do
        (assert (and (keyword? head)
                     (blank-sym-name? (string ":" head)))
                (string/format "Not a valid blank name: %s in blank-data: %s"
                               head blank-data))
        (def the-capture
          (cond
            # only ~ something and ' something
            (and (tuple? neck)
                 (or (= 'quasiquote (first neck))
                     (= 'quote (first neck))))
            (eval neck)
            #
            (or (number? neck) (string? neck))
            (errorf "numbers and strings don't capture: %n" neck)
            #
            (keyword? neck)
            (if (get lang-grammar neck)
              neck
              (errorf "Keyword %n not in grammar" neck))
            #
            (errorf "Unrecognized second item: %n in blank-data: %n"
                    neck blank-data)))
        [(tuple 'constant head)
         the-capture])
      #
      (errorf "Too many items, should only be 1 or 2: %n"
              (length parse-results))))
  #
  (defn make-query-peg
    [an-ast arr]
    (var saw-ws-last-time nil)
    (defn gen*
      [an-ast arr]
      (def head (first an-ast))
      (when (and (or (not= :ws/eol head)
                     (not= :ws/horiz head))
                 saw-ws-last-time)
        (set saw-ws-last-time false))
      (case head
        :code
        (each elt (drop 2 an-ast)
          (gen* elt arr))
        #
        :blob
        (array/push arr (in an-ast 2))
        :cmt/line
        (array/push arr (in an-ast 2))
        :str/dq
        (array/push arr (in an-ast 2))
        :str/bt
        (array/push arr (in an-ast 2))
        :ws/eol
        (when (not saw-ws-last-time)
          (set saw-ws-last-time true)
          (array/push arr :s+))
        :ws/horiz
        (when (not saw-ws-last-time)
          (set saw-ws-last-time true)
          (array/push arr :s+))
        #
        :blank
        (array/concat arr (parse-blank-data (in an-ast 2)))
        #
        :dl/square
        (do
          (array/push arr "[")
          (each elt (drop 2 an-ast)
            (gen* elt arr))
          (array/push arr "]"))
        :dl/round
        (do
          (array/push arr "(")
          (each elt (drop 2 an-ast)
            (gen* elt arr))
          (array/push arr ")"))
        :dl/curly
        (do
          (array/push arr "{")
          (each elt (drop 2 an-ast)
            (gen* elt arr))
          (array/push arr "}"))
        )
      #
      arr)
    #
    (gen* an-ast arr))
  #
  {:lang-grammar lang-grammar
   :loc-table loc->node
   :parse-query parse-query
   :query-grammar query-grammar
   :make-query-peg make-query-peg
   :parse-blank-data parse-blank-data})

(comment

  (def {:lang-grammar l-grammar
        :query-grammar q-grammar
        :parse-query parse-query}
    (jq/make-infra {:safe-delim `\`}))

  (get (peg/match l-grammar `2`) 3)
  # =>
  '(:blob @{:bc 1 :bl 1 :bp 0 :ec 2 :el 1 :ep 1} "2")

  (array/slice (peg/match q-grammar `(+ \a\ 2)`)
               3 (dec (- 3)))
  # =>
  '@[(:dl/round @{:bc 1 :bl 1 :bp 0 :ec 10 :el 1 :ep 9}
                (:blob @{:bc 2 :bl 1 :bp 1 :ec 3 :el 1 :ep 2} "+")
                (:ws/horiz @{:bc 3 :bl 1 :bp 2 :ec 4 :el 1 :ep 3} " ")
                (:blank @{:bc 4 :bl 1 :bp 3 :ec 7 :el 1 :ep 6} "a")
                (:ws/horiz @{:bc 7 :bl 1 :bp 6 :ec 8 :el 1 :ep 7} " ")
                (:blob @{:bc 8 :bl 1 :bp 7 :ec 9 :el 1 :ep 8} "2"))]

  (parse-query `(+ \a\ 2)`)
  # =>
  '@[:code @{:bc 1 :bl 1 :bp 0 :ec 10 :el 1 :ep 9}
     (:dl/round @{:bc 1 :bl 1 :bp 0 :ec 10 :el 1 :ep 9}
                (:blob @{:bc 2 :bl 1 :bp 1 :ec 3 :el 1 :ep 2} "+")
                (:ws/horiz @{:bc 3 :bl 1 :bp 2 :ec 4 :el 1 :ep 3} " ")
                (:blank @{:bc 4 :bl 1 :bp 3 :ec 7 :el 1 :ep 6} "a")
                (:ws/horiz @{:bc 7 :bl 1 :bp 6 :ec 8 :el 1 :ep 7} " ")
                (:blob @{:bc 8 :bl 1 :bp 7 :ec 9 :el 1 :ep 8} "2"))]

  (parse-query
    ``
    janet_def(\:...\ "janet/version", \:...\);
    ``)
  # =>
  '@[:code @{:bc 1 :bl 1 :bp 0 :ec 43 :el 1 :ep 42}
     (:blob @{:bc 1 :bl 1 :bp 0 :ec 10 :el 1 :ep 9} "janet_def")
     (:dl/round
       @{:bc 10 :bl 1 :bp 9 :ec 42 :el 1 :ep 41}
       (:blank @{:bc 11 :bl 1 :bp 10 :ec 17 :el 1 :ep 16} ":...")
       (:ws/horiz @{:bc 17 :bl 1 :bp 16 :ec 18 :el 1 :ep 17} " ")
       (:str/dq @{:bc 18 :bl 1 :bp 17 :ec 33 :el 1 :ep 32}
                "\"janet/version\"")
       (:blob @{:bc 33 :bl 1 :bp 32 :ec 34 :el 1 :ep 33} ",")
       (:ws/horiz @{:bc 34 :bl 1 :bp 33 :ec 35 :el 1 :ep 34} " ")
       (:blank @{:bc 35 :bl 1 :bp 34 :ec 41 :el 1 :ep 40} ":..."))
     (:blob @{:bc 42 :bl 1 :bp 41 :ec 43 :el 1 :ep 42} ";")]

  )

(comment

  (def {:parse-blank-data parse-blank-data}
    (jq/make-infra {:safe-delim `\`}))

  (parse-blank-data `:...`)
  # =>
  '[(drop (any :input))]

  (parse-blank-data `'(sequence (range "09") (to "\n"))`)
  # =>
  '[(drop (sequence (range "09") (to "\n")))]

  # '8 -macro-expand-> (quote 8) -eval-> 8
  (parse-blank-data `'8`)
  # =>
  '[(drop 8)]

  (parse-blank-data `12`)
  # =>
  [12]

  (parse-blank-data `"i am a string"`)
  # =>
  ["i am a string"]

  (parse-blank-data `::name ~2`)
  # =>
  '[(constant ::name) 2]

  (parse-blank-data `::name '1`)
  # =>
  '[(constant ::name) 1]

  (parse-blank-data `::name ''1`)
  # =>
  '[(constant ::name) (quote 1)]

  (try
    (parse-blank-data `::name 8`)
    ([e]
      (truthy? (string/find "don't capture" e))))
  # =>
  true

  (try
    (parse-blank-data `::name "fun string"`)
    ([e]
      (truthy? (string/find "don't capture" e))))
  # =>
  true

  (parse-blank-data `::name :form`)
  # =>
  '[(constant ::name) :form]

  (parse-blank-data `:form`)
  # =>
  '[(drop :form)]

  )

# the idea in the following function is to modify a grammar that
# produces a tree of nodes from a string that represents janet code,
# and use peg/match with this modified grammar to execute our query.
#
# instead of using the capture stack to capture the desired result, a
# separate "backstack" is used to collect desired targets as tables.
#
# the ordinary capture stack is not interfered with so it can be used
# in the ordinary fashion to produce the tree of nodes.  this makes
# getting at the desired results easier, but possibly it also doesn't
# mess up the capturing process (though not sure of this latter point).
(defn jq/query
  [query-str src-str &opt opts]
  #
  (def [safe-left-delim safe-right-delim]
    [`\` `\`])
  #
  (def [blank-left blank-right]
    (if-let [[bl br] (get opts :blank-delims)]
      [bl br]
      [safe-left-delim safe-right-delim]))
  # XXX: does this help?  can anything else be done?
  (when (or (and (not= blank-left safe-left-delim)
                 (string/find safe-left-delim query-str))
            (and (not= blank-right safe-right-delim)
                 (string/find safe-right-delim query-str)))
    (eprintf ``
             query-str contains characters that should be avoided:

             query-str:

             %s

             left delim: %s
             right delim: %s
             ``
             query-str safe-left-delim safe-right-delim))
  #
  (def {:lang-grammar l-grammar
        :loc-table loc->node
        :parse-query parse-query
        :make-query-peg make-query-peg
        :query-grammar q-grammar}
    # XXX: only one delim?
    (jq/make-infra {:safe-delim safe-left-delim}))
  # XXX
  (d/deprintf "query-str: %n" query-str)
  #
  (d/deprintf "blank-delims: %n" (get opts :blank-delims))
  # XXX: does this handle all cases?
  (def safe-query-str
    (if-let [[left-delim right-delim] (get opts :blank-delims)]
      (->> query-str
           (string/replace-all left-delim safe-left-delim)
           (string/replace-all right-delim safe-right-delim))
      query-str))
  # XXX
  (d/deprintf "safe-query-str: %n" safe-query-str)
  (def query-tree
    (parse-query safe-query-str))
  # XXX
  (d/deprintf "query-tree: %n" query-tree)
  (def backstack @[])
  (def converted
    (make-query-peg query-tree @[]))
  # XXX
  (d/deprintf "converted: %n" converted)
  # merge successive :s+ to allow more readable queries
  (def massaged
    (reduce (fn [acc item]
              (if (and (= :s+ (last acc))
                       (= :s+ item))
                acc
                (array/push acc item)))
            @[]
            converted))
  # XXX
  (d/deprintf "massaged: %n" massaged)
  (def query-peg
    ~(cmt (sequence ,;massaged)
          ,(fn [& args]
             # XXX
             (if (empty? args)
               (d/deprint "args was empty")
               (d/deprintf "args: %n" args))
             # capture elsewhere, but only if non-empty args
             (when (not (empty? args))
               (array/push backstack (table ;args)))
             # pass-thru -- XXX: but does this really work?
             args)))
  # integrate the query-peg with the language grammar
  (def search-grammar
    (-> (struct/to-table l-grammar)
        (put :main ~(some :input))
        # add our query to the grammar
        (put :query query-peg)
        # make the query one of the items in the choice special for
        # :form so querying works on "interior" forms.  otherwise only
        # top-level captures show up.
        (put :form (let [old-form (get l-grammar :form)]
                     (tuple 'choice
                            :query
                            ;(tuple/slice old-form 1))))))
  # XXX: affects loc->node content
  #(d/deprintf "parsing src-str with l-grammar:\n\n%n"
  #              (peg/match l-grammar src-str))
  #
  [backstack
   (peg/match search-grammar src-str)
   loc->node])

(comment

  (def query-str
    `(def \::name :blob\ \::value :input\)`)

  (def src-str
    ``
    (def a 1)

    (defn b
      [x y]
      (def c [2 3]))

    (b)

    (def x :a)
    ``)

  (def [results _ loc->node]
    (jq/query query-str src-str {:blank-delims [`\` `\`]}))

  (length results)
  # =>
  3

  (length loc->node)
  # =>
  33

  (has-key? loc->node {:bc 8 :bl 5 :bp 34
                       :ec 9 :el 5 :ep 35})
  # =>
  true

  (get loc->node {:bc 8 :bl 5 :bp 34
                  :ec 9 :el 5 :ep 35})
  # =>
  '(:blob @{:bc 8 :bl 5 :bp 34
            :ec 9 :el 5 :ep 35}
          "c")

  results
  # =>
  '@[@{::name
       (:blob @{:bc 6 :bl 1 :bp 5 :ec 7 :el 1 :ep 6} "a")
       ::value
       (:blob @{:bc 8 :bl 1 :bp 7 :ec 9 :el 1 :ep 8} "1")}
     @{::name
       (:blob @{:bc 8 :bl 5 :bp 34 :ec 9 :el 5 :ep 35} "c")
       ::value
       (:dl/square @{:bc 10 :bl 5 :bp 36 :ec 15 :el 5 :ep 41}
                   (:blob @{:bc 11 :bl 5 :bp 37 :ec 12 :el 5 :ep 38} "2")
                   (:ws/horiz @{:bc 12 :bl 5 :bp 38 :ec 13 :el 5 :ep 39} " ")
                   (:blob @{:bc 13 :bl 5 :bp 39 :ec 14 :el 5 :ep 40} "3"))}
     @{::name
       (:blob @{:bc 6 :bl 9 :bp 55 :ec 7 :el 9 :ep 56} "x")
       ::value
       (:blob @{:bc 8 :bl 9 :bp 57 :ec 10 :el 9 :ep 59} ":a")}]

  (get loc->node {:bc 8 :bl 1 :bp 7 :ec 9 :el 1 :ep 8})
  # =>
  '(:blob @{:bc 8 :bl 1 :bp 7
            :ec 9 :el 1 :ep 8}
          "1")

  )



########################################################################

(defn ij/find-janet-tags
  [src]

  '(def src
     (slurp
       (string (os/getenv "HOME") "/src/janet/src/boot/boot.janet")))

  (def query-str
    ``
    (<::type '[capture [choice "defn-" "defn"
                               "defdyn"
                               "defmacro-" "defmacro"
                               "def-" "def"
                               "var-" "var"]]>
     <::name :blob>
     <:...>)
    ``)

  (def [results _ loc->node]
    (jq/query query-str src {:blank-delims [`<` `>`]}))

  (def {:grammar loc-grammar
        :issuer issue-id
        :node-table id->node
        :loc-table loc->id
        :reset reset}
    (jc/make-infra))

  (def m-raw
    (peg/match loc-grammar src))

  # bounds info at indeces 0, 1, 2, and last 3 elements, so slice
  (def m
    (array/slice m-raw 3 (dec (- 3))))

  (def filtered
    (filter (fn [res]
              (def [_ attrs _] (get res ::name))
              (def loc (freeze attrs))
              (def id (loc->id loc))
              (unless id
                (eprintf "no id for loc: %p" loc)
                (break))
              (def parent-tuple
                (jc/up (jc/make-cursor id->node
                                       (get id->node id))))
              (unless parent-tuple (break))
              (def grent-tuple
                (jc/up parent-tuple))
              (unless grent-tuple
                # top-level
                (break true))
              # should succeed given how we got here from below
              (def head-node
                ((jc/down grent-tuple) :node))
              (def [_ _ head-name] head-node)
              # XXX: any other things (e.g. compif)?
              (= "compwhen" head-name))
            results))

  (idx/get-first-lines-and-offsets! src filtered ::name)

  # input:
  #
  # (@{"name" (:symbol @{:bc 6 :bl 10 :ec 10 :el 10} "defn")
  #    :first-line ..
  #    :offset ..
  #    "type" "def"} ...
  #
  # output:
  #
  #  @[["(def defn :macro"
  #    "defn"
  #    (string 10)
  #    (string 106)]
  #    ...]
  #
  (def results
    (seq [tbl :in filtered
          :let [first-line (get tbl :first-line)
                [_ attrs id] (get tbl ::name)
                line-no (get attrs :bl)
                offset (get tbl :offset)]]
      # XXX: hack to capture all ids in an array
      (array/push (dyn :all-ids) id)
      [first-line
       id
       (string line-no)
       (string offset)]))

  results)

########################################################################

(defn ij/index-janet-boot!
  [out-buf]
  (def boot-janet-path "src/boot/boot.janet")
  (def src
    (slurp boot-janet-path))
  #
  (idx/index-file! src boot-janet-path ij/find-janet-tags out-buf))

(comment import ./tags :prefix "")
(comment import ./etags :prefix "")
# TAGS (emacs tags file format)

```

path,line

path,line
search-string,idline,offset-from-start
...
search-string,idline,offset-from-start

```

```
 - 0x01
 - 0x0c
 - 0x7f
```

# SOH - start of heading
(def etags/start-of-heading
  (string/from-bytes 0x01))

# FF - form feed
(def etags/form-feed
  (string/from-bytes 0x0C))

# DEL - delete
(def etags/delete
  (string/from-bytes 0x7F))

(def etags/etags-grammar
  ~{:main (sequence (any (sequence :section-sep :section)) -1)
    :section-sep (sequence ,etags/form-feed :eol)
    :section (cmt (sequence :file-line (any :tag-line))
                  ,(fn [path & rest]
                     (merge ;(keep (fn [m]
                                     # each m has only one key, the id
                                     (when-let [id (first (keys m))
                                                val (get m id)]
                                       (put m
                                            id (array/push val path))))
                                   rest))))
    :file-line (sequence (capture :path) "," :d+ :eol)
    # \r, \n are here to bound the matching to the current line
    :path (some (if-not (set ",\r\n") 1))
    :tag-line (cmt (sequence (capture :search-str)
                             :tag-line-sep-1
                             (capture :id)
                             :tag-line-sep-2
                             (number :d+)
                             ","
                             (opt (number :d+))
                             :eol)
                   # slightly complicated because offset is now optional
                   ,(fn [& caps]
                      @{(get caps 1)
                        @[;(array/slice caps 2) (get caps 0)]}))
    # \r, \n are here to bound the matching to the current line
    :search-str (some (if-not (choice :tag-line-sep-1 :eol) 1))
    # \r, \n are here to bound the matching to the current line
    :id (some (if-not (choice :tag-line-sep-2 :eol) 1))
    :eol (choice "\r\n" "\r" "\n")
    :tag-line-sep-1 ,etags/delete
    :tag-line-sep-2 ,etags/start-of-heading})

(comment

  (def etags
    ```
    
    src/core/pp.c,0
    
    src/core/tuple.c,275
    JANET_CORE_FN(cfun_tuple_brackets,tuple/brackets58,2136
    JANET_CORE_FN(cfun_tuple_slice,tuple/slice66,2433
    JANET_CORE_FN(cfun_tuple_type,tuple/type80,3259
    JANET_CORE_FN(cfun_tuple_sourcemap,tuple/sourcemap96,3928
    JANET_CORE_FN(cfun_tuple_setmap,tuple/setmap108,4432
    
    src/core/regalloc.c,0
    
    src/core/specials.c,0

    ```)

  (peg/match etags/etags-grammar etags)
  # =>
  '@[@{}
     @{"tuple/brackets"
       @[58 2136 "JANET_CORE_FN(cfun_tuple_brackets," "src/core/tuple.c"]
       "tuple/setmap"
       @[108 4432 "JANET_CORE_FN(cfun_tuple_setmap," "src/core/tuple.c"]
       "tuple/slice"
       @[66 2433 "JANET_CORE_FN(cfun_tuple_slice," "src/core/tuple.c"]
       "tuple/sourcemap"
       @[96 3928 "JANET_CORE_FN(cfun_tuple_sourcemap," "src/core/tuple.c"]
       "tuple/type"
       @[80 3259 "JANET_CORE_FN(cfun_tuple_type," "src/core/tuple.c"]}
     @{}
     @{}]

  (def etags
    ```
    
    src/core/pp.c,0
    
    src/core/tuple.c,259
    JANET_CORE_FN(cfun_tuple_brackets,tuple/brackets58,
    JANET_CORE_FN(cfun_tuple_slice,tuple/slice66,
    JANET_CORE_FN(cfun_tuple_type,tuple/type80,
    JANET_CORE_FN(cfun_tuple_sourcemap,tuple/sourcemap96,
    JANET_CORE_FN(cfun_tuple_setmap,tuple/setmap108,
    
    src/core/regalloc.c,0
    
    src/core/specials.c,0

    ```)

  (peg/match etags/etags-grammar etags)
  # =>
  '@[@{}
     @{"tuple/brackets"
       @[58 "JANET_CORE_FN(cfun_tuple_brackets," "src/core/tuple.c"]
       "tuple/setmap"
       @[108 "JANET_CORE_FN(cfun_tuple_setmap," "src/core/tuple.c"]
       "tuple/slice"
       @[66 "JANET_CORE_FN(cfun_tuple_slice," "src/core/tuple.c"]
       "tuple/sourcemap"
       @[96 "JANET_CORE_FN(cfun_tuple_sourcemap," "src/core/tuple.c"]
       "tuple/type"
       @[80 "JANET_CORE_FN(cfun_tuple_type," "src/core/tuple.c"]}
     @{}
     @{}]

  )


(def tags/parse-peg
  '(sequence :s*
             "("
             (capture (to :s))
             :s))

(defn tags/to-tags-kind
  [text]
  (def compiled-peg
    (peg/compile tags/parse-peg))
  (if-let [[extracted] (peg/match compiled-peg text)]
    (case extracted
      "def" "d"
      "def-" "D"
      "defglobal" "g"
      "varglobal" "G"
      "defmacro" "m"
      "defmacro-" "M"
      "defn" "n"
      "defn-" "N"
      "varfn" "r"
      "var" "v"
      "var-" "V"
      "defdyn" "y"
      (errorf "Unexpected item: %p" extracted))
    "f"))

(comment

  (tags/to-tags-kind
    "JANET_CORE_FN(cfun_tuple_brackets,")
  # =>
  "f"

  (tags/to-tags-kind
    "(def defn :macro")
  # =>
  "d"

  (tags/to-tags-kind
    "  (defn .disasm")
  # =>
  "n"

  (tags/to-tags-kind
    (string `  `
            `(defdyn *ffi-context* " `
            `Current native library for ffi/bind and other settings")`))
  # =>
  "y"

  )

(comment

  '@[@{}
     @{"tuple/brackets"
       @[58 2136 "JANET_CORE_FN(cfun_tuple_brackets," "src/core/tuple.c"]
       "tuple/setmap"
       @[108 4432 "JANET_CORE_FN(cfun_tuple_setmap," "src/core/tuple.c"]
       "tuple/slice"
       @[66 2433 "JANET_CORE_FN(cfun_tuple_slice," "src/core/tuple.c"]
       "tuple/sourcemap"
       @[96 3928 "JANET_CORE_FN(cfun_tuple_sourcemap," "src/core/tuple.c"]
       "tuple/type"
       @[80 3259 "JANET_CORE_FN(cfun_tuple_type," "src/core/tuple.c"]}
     @{}
     @{}]

  '@[@{}
     @{"tuple/brackets"
       @[58 "JANET_CORE_FN(cfun_tuple_brackets," "src/core/tuple.c"]
       "tuple/setmap"
       @[108 "JANET_CORE_FN(cfun_tuple_setmap," "src/core/tuple.c"]
       "tuple/slice"
       @[66 "JANET_CORE_FN(cfun_tuple_slice," "src/core/tuple.c"]
       "tuple/sourcemap"
       @[96 "JANET_CORE_FN(cfun_tuple_sourcemap," "src/core/tuple.c"]
       "tuple/type"
       @[80 "JANET_CORE_FN(cfun_tuple_type," "src/core/tuple.c"]}
     @{}
     @{}]

  )

(defn tags/etags-to-tags
  [etags-buf]
  (def out-lines @[])
  (def parsed
    (peg/match etags/etags-grammar etags-buf))
  (each dict parsed
    (eachp [id info] dict
      (def line (first info))
      (def text
        (if (= 3 (length info))
          (get info 1)
          (get info 2)))
      (def path (last info))
      # XXX: tabs in text could cause problems if instead of line
      #      text converted to a regular expression is used
      (array/push out-lines
                  (string id "\t"
                          path "\t"
                          line "\t"
                          `;" ` (tags/to-tags-kind text)))))
  #
  (sort out-lines))

(comment

  (def etags-buf
    @``
     
     src/boot/boot.janet,15056
     (def defn :macrodefn10,106
     (defn defmacro :macrodefmacro45,1087
     (defmacro as-macroas-macro51,1265
     (defmacro defmacro-defmacro-59,1557

     ``)

  (tags/etags-to-tags etags-buf)
  # =>
  '@["as-macro\tsrc/boot/boot.janet\t51\t;\" m"
     "defmacro\tsrc/boot/boot.janet\t45\t;\" n"
     "defmacro-\tsrc/boot/boot.janet\t59\t;\" m"
     "defn\tsrc/boot/boot.janet\t10\t;\" d"]

  )



(def version "DEVEL")

(def usage
  ``
  Usage: idx.janet

  Generate `tags` / `TAGS` file for Janet source code

  Invoke in root of janet source repository directory.

  This handles lookups for:

  * Janet -> Janet  (e.g. defn in boot.janet)
  * Janet -> C      (e.g. set in specials.c)

  For C -> C lookups, consider an LSP server for C such as ccls or
  clangd.

  By default a `tags` file is generated.

  To create a `TAGS` instead (e.g. for use with emacs), set the
  `IJ_OUTPUT_FORMAT` environment variable to have the value `etags`,
  before invoking `idx.janet`.

  For example, on a *nix machine with certain shells, this could be
  something like:

    export IJ_OUTPUT_FORMAT=etags

  Other systems and/or shells may have a different way of setting
  environment variables.
  ``)

########################################################################

(defn in-janet-src-dir?
  []
  (and (os/stat "janet.1")
       (os/stat "src")))

(defn file-newest?
  [file-path dir-path]
  (def tags-mtime
    (get (os/stat file-path) :modified))
  (var newest-path file-path)
  (with [of (file/temp)]
    (def dir (os/cwd))
    (defer (os/cd dir)
      (os/cd dir-path)
      (def proc (os/execute ["git" "ls-files"] :px
                            {:out of}))
      # XXX: unneeded?
      (file/flush of)
      (file/seek of :set 0)
      (def content (string/trim (file/read of :all)))
      (def lines (string/split "\n" content))
      (each res lines
        (def mtime (get (os/stat res) :modified))
        (when (> mtime tags-mtime)
          (set newest-path res)
          (break)))))
  #
  (= newest-path file-path))

(defn all-ids-valid?
  [all-ids]
  (and (array? all-ids)
       (all string? all-ids)))

(comment

  (all-ids-valid? @["alice" "bob" "carol"])
  # =>
  true

  (all-ids-valid? [:a :b :c])
  # =>
  false

  (all-ids-valid? @["tom" :wall "jerry"])
  # =>
  false

  )

########################################################################

(defn main
  [& argv]

  (when (or (not (in-janet-src-dir?))
            (when-let [arg (get argv 1)]
              (= "--help" arg)))
    (print usage)
    (break 0))

  (def opts
    @{:output-format "u-ctags"
      :file-extension ""})

  (when (os/getenv "IJ_C2C")
    (setdyn :ij-c2c true))

  (when (os/getenv "IJ_DEBUG")
    (setdyn :ij-debug true))

  (when-let [fmt (os/getenv "IJ_OUTPUT_FORMAT")]
    (when (nil? (get {"etags" true "u-ctags" true}
                     fmt))
      (errorf "Unrecognized IJ_OUTPUT_FORMAT value: %s" fmt))
    (put opts :output-format fmt))

  (def out-format
    (opts :output-format))

  (when-let [file-ext (os/getenv "IJ_FILE_EXTENSION")]
    (put opts :file-extension file-ext))

  (def file-extension
    (opts :file-extension))

  (def tags-fname
    (case out-format
      "etags"
      (string "TAGS" file-extension)
      #
      "u-ctags"
      (string "tags" file-extension)
      #
      (errorf "Unrecognized output-format: %s" out-format)))

  (def out-buf @"")

  # XXX: eventually index other janet files in source tree too?
  #      only seemed to ever index boot.janet

  # XXX: hack to capture all ids in an array
  (setdyn :all-ids @[])

  (ij/index-janet-boot! out-buf)

  (each name (os/dir "src/core/")
    (def path (string "src/core/" name))
    (def src (slurp path))
    (cond
      (= "io.c" name)
      (ij2c/index-janet-core-def-c! src path out-buf)
      #
      (= "math.c" name)
      (do
        (ij2c/index-math-c! src path out-buf)
        (ij2c/index-janet-core-def-c! src path out-buf))
      #
      (= "specials.c" name)
      (ij2c/index-specials-c! src path out-buf)
      #
      (= "corelib.c" name)
      (ij2c/index-corelib-c! src path out-buf))
    #
    (try
      (ij2c/index-generic-c! src path out-buf)
      ([e]
        (eprintf "%s %s" e path)))
    #
    (when (dyn :ij-c2c)
      (try
        (ic/index-c! src path out-buf)
        ([e]
          (eprintf "%s %s" e path)))))

  (when (dyn :ij-c2c)
    (def path
      "src/include/janet.h")
    (def src
      (slurp path))
    (try
      (ic/index-c! src path out-buf)
      ([e]
        (eprintf "%s %s" e path))))

  (def out-lines
    (if (= out-format "u-ctags")
      (tags/etags-to-tags out-buf)
      (string/split "\n" out-buf)))

  # write the index (u-ctags -> tags, etags -> TAGS)
  (with [tf (file/open tags-fname :w)]
    # XXX: yuck -- if a toggling sorting option is provided, following code
    #      probably needs to change
    (when (= out-format "u-ctags")
      (file/write tf
                  (string "!_TAG_FILE_SORTED\t"
                          "1\t"
                          "/0=unsorted, 1=sorted, 2=foldcase/\n")))
    (each line out-lines
      (when (not= line "") # XXX: not nice to be checking so many times
        (file/write tf line)
        (when (not (or (string/has-suffix? "\r" line)
                       (string/has-suffix? "\n" line)))
          (file/write tf "\n"))))
    (file/flush tf)))

########################################################################

(defn build-index
  [j-src-path file-ext &opt format]
  (default format "etags")
  (def dir (os/cwd))
  (defer (os/cd dir)
    (os/cd j-src-path)
    (os/setenv "IJ_OUTPUT_FORMAT" format)
    (os/setenv "IJ_FILE_EXTENSION" file-ext)
    (main)))

