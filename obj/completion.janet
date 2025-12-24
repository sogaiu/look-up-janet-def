(def compl/bash-completion
  ``
  _lujd_ids() {
      COMPREPLY=( $(compgen -W "$(lujd --raw-all)" -- ${COMP_WORDS[COMP_CWORD]}) );
  }
  complete -F _lujd_ids lujd
  ``)

(def compl/fish-completion
  ``
  function __lujd_complete_ids
    if not test "$__lujd_ids"
      set -g __lujd_ids (lujd --raw-all)
    end

    printf "%s\n" $__lujd_ids
  end

  complete -c lujd -a "(__lujd_complete_ids)" -d 'ids'
  ``)

(def compl/zsh-completion
  ``
  #compdef lujd

  _lujd() {
      local matches=(`lujd --raw-all`)
      compadd -a matches
  }

  _lujd "$@"
  ``)

(defn compl/maybe-handle-dump-completion
  [opts]
  # this makes use of the fact that print returns nil
  (not
    (cond
      (opts :bash-completion)
      (print compl/bash-completion)
      #
      (opts :fish-completion)
      (print compl/fish-completion)
      #
      (opts :zsh-completion)
      (print compl/zsh-completion)
      #
      true)))

