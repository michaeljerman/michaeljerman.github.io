<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">

    <title> ECON 383 </title>

    <link rel="stylesheet" href="../../reveal/css/reveal.css">
    <link rel="stylesheet" href="../../reveal/css/theme/slide_white.css">

    <!-- Theme used for syntax highlighting of code -->
    <link rel="stylesheet" href="../../reveal/lib/css/zenburn.css">

    <!-- Printing and PDF exports -->
    <script>
      var link = document.createElement( 'link' );
      link.rel = 'stylesheet';
      link.type = 'text/css';
      link.href = window.location.search.match( /print-pdf/gi ) ? '../../reveal/css/print/pdf.css' : '../../reveal/css/print/paper.css';
      document.getElementsByTagName( 'head' )[0].appendChild( link );
    </script>
  </head>
  <body>
    <div class="reveal">
      <div class="slides">

	<!-- C-x C-k S for slide, M for markdown -->

	<section data-markdown>
	  # title

	  ## ECON 383: Economics of Discrimination

	  ## Winter 2018
	</section>

	<section data-markdown>
	  ### Reading

	  - Textbook, chapter 1 
	</section>
	
      </div>
    </div>

    <script src="../../reveal/lib/js/head.min.js"></script>
    <script src="../../reveal/js/reveal.js"></script>

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
      { src: '../../reveal/plugin/markdown/marked.js' },
      { src: '../../reveal/plugin/markdown/markdown.js' },
      { src: '../../reveal/plugin/notes/notes.js', async: true },
      { src: '../../reveal/plugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } },
      { src: '../../reveal/plugin/math-katex/math-katex.js', async: true }
      ],

      
      });
    </script>
  </body>
</html>
