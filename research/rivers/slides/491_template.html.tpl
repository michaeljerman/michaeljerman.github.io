<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">

    <title> EC 491: Introduction </title>

    <link rel="stylesheet" href="css/reveal.css">
    <link rel="stylesheet" href="css/theme/slide_white.css">

    <!-- Theme used for syntax highlighting of code -->
    <link rel="stylesheet" href="lib/css/zenburn.css">

    <!-- Printing and PDF exports -->
    <script>
      var link = document.createElement( 'link' );
      link.rel = 'stylesheet';
      link.type = 'text/css';
      link.href = window.location.search.match( /print-pdf/gi ) ? 'css/print/pdf.css' : 'css/print/paper.css';
      document.getElementsByTagName( 'head' )[0].appendChild( link );
    </script>
  </head>
  <body>
    <div class="reveal">
      <div class="slides">

	<!-- C-x C-k S for slide, M for markdown -->

	<section data-markdown>
	  # Introduction

	  ## EC 491: Economic Growth

	  ## Summer 2017
	</section>

	<section data-markdown>
	  ### Reading

	  - Textbook, chapter 1 
	</section>
	
      </div>
    </div>

    <script src="lib/js/head.min.js"></script>
    <script src="js/reveal.js"></script>

    <script>
      // More info https://github.com/hakimel/reveal.js#configuration
      Reveal.initialize({
      history: true,
      backgroundTransition: 'none',
      transition: 'fade',
      viewDistance: 2,
      controls: true,
      progress: true,
      center: true,

      // More info https://github.com/hakimel/reveal.js#dependencies
      dependencies: [
      { src: 'plugin/title-footer/title-footer.js', async: true, callback: function() { title_footer.initialize('Lecture Slides'); } },
      { src: 'plugin/chalkboard/chalkboard.js' },
      { src: 'plugin/markdown/marked.js' },
      { src: 'plugin/markdown/markdown.js' },
      { src: 'plugin/notes/notes.js', async: true },
      { src: 'plugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } },
      { src: 'plugin/math-katex/math-katex.js', async: true }
      ],

      keyboard: {
        67: function() { RevealChalkboard.toggleNotesCanvas() },
        66: function() { RevealChalkboard.toggleChalkboard() }, 
        46: function() { RevealChalkboard.clear() },    
         8: function() { RevealChalkboard.reset() },    
        68: function() { RevealChalkboard.download() }, 
      },

      chalkboard: { 
      theme: "whiteboard",
      color: [ 'rgba(0,0,0,1)', 'rgba(255,255,255,0.5)' ],
      pen: [ 'crosshair', 'crosshair' ]
      },
      
      });
    </script>
  </body>
</html>
