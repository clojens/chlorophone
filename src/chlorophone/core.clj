(ns chlorophone.core
  (:require (clojure [pprint :refer [print-table]])))

(defrecord Phoneme [sampa ipa description examples vulgar])

(defn vowel-modifiers
  []
  {:nasalised {:keys [[:tilde] :or [:underscore :tilde]]
               :doc "After a vowel indicates that it is nasalised (e.g. French bon [bO~] )."}
   :lengthened {:keys [:colon]
                :doc "After vowel indicates lengthened (Japanese shōshō [So:So:], English see [si:] )."}
   :rhoticity {:keys [:backtick]
               :doc "After a vowel indicates rhoticity (e.g. US English bird [b3`d] )."}
   :nonsyllabic {:keys [:underscore :caret]
                 :docs "Non syllabic vowel (IPA subscript arch)"}})

;; (print-table (inject-vowels))

(defn consonants
  "Inject the consonant table entries as Phoneme records containing SAMPA and IP
  for phonetic unit notation, the first based solely on ANSI characters whereas
  the last one (IPA) isn't as it contains Cyrillic characters among others."
  []
  (map #(->Phoneme (% 0) (% 1) (% 2) (% 3))
       [["p" "p" [:voiceless :bilabial :plosive] "English [p]en"]
        ["b" "b" [:voiced :bilabial :plosive] "English [b]ut"]
        ["t" "t" [:voiceless :alveolar :plosive] "English [t]wo, Spanish [t]oma ('capture')"]
        ["d" "d" [:voiced :alveolar :plosive] "English [d]o, Italian ca[d]e"]
        ["ts" "ts" [:voiceless :alveolar :affricate] "Italian cal[z]a, German [z]eit"]
        ["dz" "dz" [:voiced :alveolar :affricate] "Italian [z]ona ('zone')"]
        ["tS" "tʃ" [:voiceless :postalveolar :affricate]	"English [ch]air, , Spanish mu[ch]o ('many')"]
        ["dZ" "dʒ" [:voiced :postalveolar :affricate] "English [g]in, Italian [gi]orno"]
        ["c" "c" [:voiceless :palatal :plosive] "Hungarian [ty]úk 'hen'"]
        ["J\\" "ɟ" [:voiced :palatal :plosive] "Hungarian e[gy] 'one'"]
        ["k" "k" [:voiceless :velar :plosive] "English s[k]ill"]
        ["g" "ɡ" [:voiced :velar :plosive] "English [g]o"]
        ["q" "q" [:voiceless :uvular :plosive] "Arabic [q]of"]
        ["p\\" "ɸ" [:voiceless :bilabial :fricative] "Japanese [f]u"]
        ["B" "β" [:voiced :bilabial :fricative] "Catalan ro[b]a 'clothes'"]
        ["f" "f" [:voiceless :labiodental :fricative]	"English [f]ool, Spanish and Italian [f]also ('false')"]
        ["v" "v" [:voiced :labiodental :fricative] "English [v]oice, German [W]elt"]
        ["T" "θ" [:voiceless :dental :fricative] "English [th]ing, Castilian Spanish ca[z]a"]
        ["D" "ð" [:voiced :dental :fricative] "English [th]is"]
        ["s" "s" [:voiceless :alveolar :fricative] "English [s]ee, Spanish [s]í ('yes')"]
        ["z" "z" [:voiced :alveolar :fricative] "English [z]oo, German [S]ee"]
        ["S" "ʃ" [:voiceless :postalveolar :fricative]	"English [sh]e, French [ch]emin"]
        ["Z" "ʒ" [:voiced :postalveolar :fricative] "French [j]our, English plea[s]ure"]
        ["C" "ç" [:voiceless :palatal :fricative] "Standard German I[ch]"]
        ["j\\ (jj)"	"ʝ"	[:voiced :palatal :fricative] "Standard Spanish a[y]uda"]
        ["x" "x" [:voiceless :velar :fricative] "Scots lo[ch], Castilian Spanish a[j]o"]
        ["G" "ɣ" [:voiced :velar :fricative] "Greek γάλα ('milk')"]
        ["" "ɰ" [:velar :approximant] "Spanish al[g]o"]
        ["X\\" "ħ" [:voiceless :pharyngeal :fricative] "Arabic [h].â"]
        ["?\\" "ʕ" [:voiced :pharyngeal :fricative] "Arabic 'ayn"]
        ["h" "h" [:voiceless :glottal :fricative] "English [h]am, German [H]and"]
        ["h\\" "ɦ" [:voiced :glottal :fricative] "Hungarian le[h]et"]
        ["m" "m" [:bilabial :nasal] "English [m]an"]
        ["F" "ɱ" [:labiodental :nasal] "Spanish i[n]fierno, Hungarian ká[m]for"]
        ["n" "n" [:alveolar :nasal] "English, Spanish and Italian [n]o"]
        ["J" "ɲ" [:palatal :nasal] "Spanish a[ñ]o, French oi[gn]on"]
        ["N" "ŋ" [:velar :nasal] "English ri[ng], Italian bia[n]co, Tagalog [ng]ayón"]
        ["l" "l" [:alveolar :lateral :approximant] "English [l]eft, Spanish [l]argo"]
        ["L" "ʎ" [:palatal :lateral :approximant] "Italian a[gl]io, Catalan co[ll]a,"]
        ["5" "ɫ" [:velarized :dental :lateral] "English mea[l] Catalan a[l]ga"]
        ["4 (r)"	"ɾ" [:alveolar :tap] "Spanish pe[r]o, Italian esse[r]e"]
        ["r (rr)"	"r" [:alveolar :trill] "Spanish pe[rr]o"]
        ["r\\" "ɹ" [:alveolar :approximant] "English [r]un"]
        ["R" "ʀ" [:uvular :trill] "Standard German [R]eich"]
        ["P" "ʋ" [:labiodental :approximant] "Dutch [W]aar"]
        ["w" "w" [:labial:-velar :approximant] "English [w]e, French [ou]i"]
        ["H" "ɥ" [:labial:-palatal :approximant] "French h[u]it"]
        ["j" "j" [:palatal :approximant] "English [y]es, French [y]eux"]]))

(->Phoneme nil nil nil nil nil)

(defn vowels
  "* = somewhat more centralised and relaxed"
  []
  (map #(->Phoneme (% 0) (% 1) (% 2) (% 3) (when (not (nil? %)) (% 4)))
       [["i"	"i" [:front :closed :unrounded :vowel] "English s[ee], Spanish s[í], French v[i]te, German m[i.e.]ten, Italian v[i]sto"]
        ["I"	"ɪ" [:front :closed :unrounded :vowel*] "English c[i]ty, German m[i]t" :small-capital-I]
        ["e"	"e" [:front :half :closed :unrounded :vowel] "US English b[ea]r, Spanish [é]l, French ann[ée], German m[eh]r, Italian r[e]te, Catalan m[é]s"]
        ["E"	"ɛ" [:front :half :open :unrounded :vowel] "English b[e]d, French m[ê]me,German H[e]rr, M[ä]nner, Italian f[e]rro, Catalan m[e]s, Spanish p[e]rro"]
        ["{"	"æ" [:front :open :unrounded :vowel] "English c[a]t" :ae-ligature]
        ["y"	"y" [:front :closed :rounded :vowel] "French d[u], German T[ü]r"]
        ["2"	"ø" [:front :half :closed :rounded :vowel] "French d[eu]x (hence '2'), German H[öh]le" :slashed-o]
        ["9"	"œ" [:front :half :open :rounded :vowel] "French n[e]uf (hence '9'), German H[ö]lle" :oe-ligature]
        ["1"	"i" [:central :closed :unrounded :vowel] "Russian м[ы]с [m1s] 'cape'" :overstroked-i]
        ["@"	"ə schwa" [:central :neutral :unrounded :vowel] "English [a]bout, winn[e]r,German bitt[e]" :turned-down-e]
        ["6"	"ɐ open schwa" [:central :neutral :unrounded :vowel] "German bess[er]" :turned-down-a]
        ["3"	"ɜ" [:front :half :open :unrounded :vowel] "English b[ir]d" :greek-epsilon-mirrored-left]
        ["a"	"a" [:central :open :vowel] "Spanish d[a], b[a]rra, French b[a]teau, l[a]c, German H[aa]r, Italian p[a]zzo"]
        ["}"	"ʉ" [:central :closed :rounded :vowel] "Scottish English p[oo]l, Swedish sj[u]" :overstroked-u]
        ["8"	"ɵ" [:central :neutral :rounded :vowel] "Swedish k[u]st" :overstroked-o]
        ["&"	"ɶ" [:front :open :rounded :vowel] "American English th[a]t" :small-capital-OE-ligature]
        ["M"	"ɯ" [:back :closed :unrounded :vowel] "Japanese f[u]ji, Vietnamese ư Korean 으" :upside-down-m]
        ["7"	"ɤ" [:back :half :closed :unrounded :vowel] "Vietnamese ơ Korean 어" :squeezed-Greek-gamma]
        ["V"	"ʌ" [:back :half :open :unrounded :vowel] "RP and US English r[u]n, eno[u]gh" :turned-down-v]
        ["A"	"ɑ" [:back :open :unrounded :vowel] "English [a]rm, US English l[aw], standard French [â]me" :d-with-no-upper-tail]
        ["u"	"u" [:back :closed :rounded :vowel] "English s[oo]n, Spanish t[ú], French g[oû]t, German H[u]t, M[u]tter, Italian azz[u]rro, t[u]tto"]
        ["U"	"ʊ" [:back :closed :rounded :vowel*] "English p[u]t, (non-US) B[u]ddhist" :turned-down-small-capital-Greek-omega]
        ["o"	"o" [:back :half :closed :rounded :vowel] "US English s[o]re, Scottish English b[oa]t, Spanish y[o], French b[eau], German S[oh]le, Italian d[o]ve, Catalan [o]na"]
        ["O"	"ɔ" [:back :half :open :rounded :vowel] "British English l[aw], c[au]ght, Italian c[o]sa, Catalan d[o]na, Spanish [o]jo, German W[o]rt" :c-mirrored-to-the-left]
        ["Q"	"ɒ" [:back :open :rounded :vowel] "British English n[o]t, c[ou]gh" :b-with-no-upper-tail]
        ]))

(vowels)
