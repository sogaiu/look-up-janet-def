(import ./dyns :as d)
(import ./index :as idx)
(import ./src :as src)

(def usage
  `````
  Usage: lujd identifier
         lujd [-h|--help]

  Jump to the definition of a Janet identifier.

    -h, --help                 show this output

  Look up the definition of `identifier` [1], a Janet
  identifier, and if found, open an editor [2] to
  display the located definition.

  Be careful to quote shortnames (e.g. *, ->, >, <-,
  etc.) appropriately so the shell doesn't process them
  in an undesired fashion.

  ---

  [1] Lookups are performed via an index of the Janet
  source code.  The index (a file named `TAGS.lujd`) is
  built from a local copy of the Janet source code and
  placed in the same directory.

  The location of a local copy of the Janet source code
  can be specified via a configuration file or an
  environment variable.

  For the configuration file approach, create a file
  named `.lujd.janet` in your `HOME` / `USERPROFILE`
  directory.  The content should be something like:

  ```
  {:janet-src-path
   (string (os/getenv "HOME") "/src/janet")}
  ```

  That is, the file should end with a struct that
  has at least the key `:janet-src-path` and its
  associated value should evaluate to a full path to
  Janet source code.

  For the environment variable approach, set
  `LUJD_JANET_SRC_PATH` to a full path of a local
  copy of the Janet source code.

  [2] The default editor is `nvim`.  Other supported
  editors include: `emacs`, `hx`, `kak`, `subl`, and
  `vim`.

  A particular editor other than the default can be
  configured via a file (see info about `.lujd.janet`
  above) or via an environment variable.

  For the configuration file approach, in a file
  named `.lujd.janet` in your `HOME` / `USERPROFILE`
  directory, add an appropriate key-value pair to
  a struct which ends up as the last value to be
  evaluated in the file.

  The key should be `:editor` and the value should
  be one of: `emacs`, `hx`, `kak`, `nvim`, `subl`,
  or `vim`.

  An example `.lujd.janet` might look like:

  ```
  {:editor "emacs"
   :janet-src-path
   (string (os/getenv "HOME") "/src/janet")}
  ```

  For the environment variable approach,
  `LUJD_EDITOR` should be set to one of: `emacs`,
  `hx`, `kak`, `nvim`, `subl`, or `vim`.
  `````)

########################################################################

(defn main
  [& argv]

  (d/init-dyns)

  (def thing (get argv 1))

  (when (or (not thing)
            (= "-h" thing)
            (= "--help" thing))
    (print usage)
    (os/exit 0))

  (def j-src-path (dyn :lujd-janet-src-path))
  (when (or (nil? j-src-path)
            (not= :directory (os/stat j-src-path :mode)))
    (eprint "Failed to find Janet source directory.")
    (eprint "Please set the env var LUJD_JANET_SRC_PATH to a")
    (eprint "full path of Janet source or arrange for an")
    (eprint "appropriate config file.  Please see the program")
    (eprint "usage text for details.")
    (os/exit 1))

  (def file-ext ".lujd")
  (def tags-fname (string "TAGS" file-ext))
  (def etags-file-path (string j-src-path "/" tags-fname))

  (when (or (not (os/stat etags-file-path))
            (not (idx/file-newest? etags-file-path j-src-path)))
    (eprintf "Index file might be stale or failed to find it in: %s"
             j-src-path)
    (eprintf "Trying to create fresh index file at: %s"
             etags-file-path)
    (idx/build-index j-src-path file-ext)
    (when (not (os/stat etags-file-path))
      (eprintf "Failed to create index file at: %s" etags-file-path)
      (os/exit 1))
    (printf "Created index file at: `%s`" etags-file-path))

  (def etags-content
    (try
      (slurp etags-file-path)
      ([e]
        (eprintf "Failed to read `%s`: %s"
                 etags-file-path etags-file-path)
        (os/exit 1))))

  (src/definition thing etags-content j-src-path))
