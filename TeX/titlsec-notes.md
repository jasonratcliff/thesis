
\\titleformat{command}
  [shape]{format}{label}{sep}{before-code}[after-code]
  
  * <command> points to the section to be redefined:
  
      - \part
      - \chapter
      - \section
      - \subsection
      - \paragraph
      - \subparagraph
      
  * <shape> defines the distribution of elements for the title:
  
      - hang - Default, hanging label
      - block - Whole title is typeset as a single block (i.e. a paragpraph)
      - display - Title label is put in a separate paragraph
      
  * <format> sets format applied to the entire title - label and text
      
      - e.g. {\normalsize\filcenter}
      
  * <label> defines the title label
  
      - e.g. {CHAPTER \thechapter} % sets label as CHAPTER N
  
  * <sep> sets the horizontal space between label and title body
  
    - This space is vertical in DISPLAY shape
    
  * <before-code> allows code to precede the title body
  
    - The final command 