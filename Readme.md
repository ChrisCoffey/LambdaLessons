The idea is to create a web-based interactive lambda calculus interpreter, then build up to typed lambda calculus and eventually hindley-milner inference from there. Lots of fun stuff ahead...

Step 1: The lambda calculus:

  <exp> ::= <var>
          | (<exp> <exp>)
          | (λ (<var>) <exp>)
  


[![Join the chat at https://gitter.im/ChrisCoffey/LambdaLessons](https://badges.gitter.im/ChrisCoffey/LambdaLessons.svg)](https://gitter.im/ChrisCoffey/LambdaLessons?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)