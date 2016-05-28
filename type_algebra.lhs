%options ghci -fglasgow-exts

%if False

> {-# OPTIONS -XStandaloneDeriving -XFlexibleInstances -XFlexibleContexts #-}
> module TypeAlgebra where
>
> import Prelude hiding (Maybe)

%endif

\documentclass[handout]{beamer}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt
%include beamer.fmt

\usepackage{appendixnumberbeamer}

\usetheme{metropolis}
\definecolor{BerkeleyBlue}{RGB}{0,50,98}
\definecolor{FoundersRock}{RGB}{59,126,161}
\definecolor{Medalist}{RGB}{196,130,14}
\setbeamercolor{frametitle}{fg=white,bg=FoundersRock}
\setbeamercolor{title separator}{fg=Medalist,bg=white}
% \usefonttheme[onlymath]{serif}
\usefonttheme[onlymath]{serif}

\usepackage[macros]{acro}
\acsetup{short-format = \scshape}

\DeclareAcronym{adt}{short = adt, long = algebraic data type}

\usepackage{amsmath,dsfont}
\usepackage{xspace}

\newcommand{\reals}{\ensuremath{\mathds{R}\xspace}}
\newcommand{\bools}{\ensuremath{\mathds{B}\xspace}}
\newcommand{\sets}{\ensuremath{\mathcal{S}\xspace}}
\newcommand{\hask}{\ensuremath{\mathcal{H}\xspace}}

%% \newcommand{\unmu}{{\mathpalette\umu\relax}}
%% \newcommand{\umu}[2]{\raisebox{\depth}{\scalebox{1}[-1]{$#1\mu$}}}
\newcommand{\unmu}{\ensuremath{\hat{\mu}\xspace}}

\newcommand{\vpause}{\vspace*{-\baselineskip}\pause}

\usepackage{tikz}
\def\checkmark{\tikz\fill[scale=0.4](0,.35) -- (.25,0) -- (1,.7) -- (.25,.15) -- cycle;}
\def\disst{\tikz[scale=0.15]{\draw[thick] (-1,0) -- (1,0) -- (0,2) -- cycle;\draw[thick] (0, 2) -- (0, 0);}}
\def\clownt{\tikz[scale=0.15]{\draw[thick] (0, 2) -- (-1,0) -- (0,0);}}
\def\jokert{\tikz[scale=0.15]{\draw[thick] (0, 2) -- (1,0) -- (0,0);}}

\newcommand{\diss}{\ensuremath{\raisebox{-.3ex}{\disst\,}}}
\newcommand{\clown}{\ensuremath{\raisebox{-.3ex}{\clownt\,}}}
\newcommand{\joker}{\ensuremath{\raisebox{-.3ex}{\jokert\,}}}

%if False

> type Real = Float
> bottom = undefined

%endif

%format bottom = "\perp"

%format Real = "\reals"
%format Bool = "\bools"
%format Set = "\sets"
%format && = "\wedge"
%format || = "\vee"

\AtBeginSection[]
{
  \begin{frame}
    \frametitle{Table of Contents}
    \tableofcontents[currentsection]
  \end{frame}
}

\title{The Algebra and Dissection of Types}
\subtitle{\verb|github.com/ryanorendorff/algebra_lambdaconf2016|}
\author{Ryan~Orendorff\inst{1,2}}

\institute{
  \inst{1}%
  Department of Bioengineering\\
  University of California, Berkeley\\
  University of California, San Francisco
  \and
  \inst{2}
  Innolitics LLC\\
  Medical Imaging Software Consultants
}
\date{May 2016}

\begin{document}

\frame{\titlepage}

\section{Overview of Algebra}


% \begin{frame}
% \frametitle{What is an Algebra}
%
% An algebra is usually defined as
%
% \begin{quote}
% the study of mathematical symbols and the rules for manipulating these symbols.
% \end{quote}
%
% \end{frame}


\begin{frame}
\frametitle{The Reals}

A familiar set of symbols is the reals (|Real|). With the reals we can define an operation |+| that combines two reals together.

< +  ::  Real -> Real -> Real

We note that this function |+| is \emph{closed} over the set |Real|;
applying |+| to two reals gives something in the same set (another real).

\pause

We denote this algebra with the following notation for some set \sets and some operation $Op$.

< (Set, Op)

\end{frame}


\begin{frame}
\frametitle{Algebraic Structures}

Often sets and associated operators have a common thread. For example, for
$(\reals, +)$ and $(\bools, \vee)$

\pause

\begin{itemize}

  \item The operator return an element in the same set.

< +   ::  Real -> Real -> Real
< ||  ::  Bool -> Bool -> Bool

\vpause

  \item The operator is associative.

< a + (b + c)    =  (a + b) + c
< a || (b || c)  =  (a || b) || c

\vpause

  \item There exists an $e \in \sets$ such that $e \times a = a \times e = a$.

< a + 0       =  0      +   a  =  a
< a || False  =  False  ||  a  =  a

\end{itemize}

These types of algebraic structures are \emph{monoids}.

\end{frame}


%% \begin{frame}
%% \frametitle{Other Algebraic Structures}
%%
%% The following are other algebraic structures that build off each other.
%%
%% \begin{itemize}
%%   \item \emph{Magmas}: A set with a closed binary operator.
%%   \item \emph{Semigroup}: A \emph{magma} where the operator is associative.
%%   \item \emph{Monoid}: A \emph{semigroup} with an identity element.
%%   \item \emph{Group}: A \emph{monoid} with inverses.
%% \end{itemize}
%%
%% \end{frame}

\section{Algebraic Data Structures}

\begin{frame}
\frametitle{Algebraic Data Types}

In this talk we are going to focus on only \acp{adt}\footnote{we are
actually going to further restrict to regular data types}. For example, we
can define a type that holds multiple values for one constructor

> data Doubled b = Doubled b b

and we can use multiple constructors for a type.

> data Cards  = Hearts | Diamonds | Clubs | Spades

Let's look at each of these cases in turn.

% Let's look at how we can build up these types generically.\footnote{This
% process is known as pattern functors. See~\cite{bird_algebra}}

\end{frame}

%format (Sum1 (f) (g)) = f "+_{\!1}" g
%format (Prod1 (f) (g)) = f "\times_{\!1}" g
%format (P1 (x) (y)) = (x "\,," y) "_{1}"
%format L1 = L "_{1}"
%format R1 = R "_{1}"
%format Id1 = "Id_{1}"
%format K1 = "K_{1}"

%format (Prod (f) (g)) = f "\times " g
%format (ProdD (f) (g)) = f "\hat{\times} " g
%format (Sum (f) (g)) = f "+ " g
%format Sum' = Sum
%format L (f) = "L\, " f
%format R (f) = "R\, " f

%format Id = "Id"
%format K = "K"

%format IdD = "\hat{Id}"
%format KD = "\hat{K}"

%format (Diff (f)) = "\partial " f
%format (Diss (f)) = "\diss " f

%format (clown (f)) = "\clown " f
%format (joker (f)) = "\joker " f
%format (Clown (f)) = "\clown " f
%format (Joker (f)) = "\joker " f

%format One  = "1"
%format OneD = "\hat{1}"
%format Zero = "0"

%format One1 = "1_{1}"

%format `iso` = "\cong"
%format `eq` = "="

\begin{frame}
\frametitle{Products}

We can define a product type as follows, where we hold onto two values.

> data Product a b = Product a b

\vpause

Which is equivalent to a pair tuple.

< Product a b `iso` (a, b) `eq` (,) a b

\vpause

We can write the same thing in a slightly different way.

> data Prod a b = ProdD a b

\end{frame}

\begin{frame}
\frametitle{The One}

Now let's do a little trick. Let's define the following data type.

> data One = OneD

We can only construct this value in one way. How many ways are there to
construct this?

< Prod One a

\vpause

Well |One| only has one value (|OneD|), so we can think of the following
relation to hold\footnote{($\cong$) is usually read as "equal up to
isomorphism"}.

< Prod One a `iso` a `iso` Prod a One


\end{frame}


\begin{frame}
\frametitle{Product is a monoid!}

Remember that a monoid has to follow the following rules.

\pause

\begin{itemize}[<+->]

  \item The operator return an element in the same set\footnote{The star
        |*| represents a kind.}.

< Prod a b ::  * -> * -> *

  \item The operator is associative.

< Prod a ((Prod b c)) `iso` Prod ((Prod a b)) c

  \item There exists an $e \in \sets$ such that $e \cdot a = a \cdot e = a$.

< Prod a One `iso` Prod One a `iso` a

\end{itemize}

\end{frame}


\begin{frame}
\frametitle{Sums}

We can define a sum type as follows, where we hold onto either one value or
a different value.

Which is equivalent to this.

< Sum' a b `iso` a | b

\vpause

We can write the same thing in a slightly different way.

> data Sum a b = L a | R b

\end{frame}

\begin{frame}
\frametitle{Zero is rather empty}

Now let's do a little trick. Let's define the following data type.

> data Zero

\vpause

How do we construct a value of this type? We can't! We could never call
the following function.

> uncallable :: Zero -> a
> uncallable x = x `seq` error ":-("

\vpause

In which case, we expect the following.

< Sum a Zero `iso` Sum Zero a `iso` a


\end{frame}


\begin{frame}
\frametitle{Sum is a monoid!}

Remember that a monoid has to follow the following rules.

\pause

\begin{itemize}[<+->]

  \item The operator return an element in the same set\footnote{The star
        |*| represents a kind.}.

< Sum a b ::  * -> * -> *

  \item The operator is associative.

< Sum a ((Sum b c)) `iso` Sum ((Sum a b)) c

  \item There exists an $e \in \sets$ such that $e \cdot a = a \cdot e = a$.

< Sum a Zero `iso` Sum Zero a `iso` a

\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Constant and Identity Types}

We are also going to define the following data types.

\pause

A constant type, which represents something like |Nil| or |5|.

> data K  x   =  KD  -- constant

\pause

An identity type.

> data Id x  =  IdD x  -- element

\end{frame}


\begin{frame}
\frametitle{Semirings}

A semiring is defined by the following set of rules.

\pause

\begin{itemize}[<+->]

  \item $(+, 0)$ is a commutative monoid.
  \item $(\times, 1)$ is a monoid.
  \item $\times$ distributes over $+$: $a \times (b + c) = a \times b + a \times c$ and $(b + c) \times a = b\times a + c\times a$.
  \item 0 is an annihilator for $\times$: $a \times 0 = 0 \times a = 0$.

\end{itemize}

\pause

\acp{adt} form a semiring!

\end{frame}


\begin{frame}
\frametitle{Star (or Closed) Semiring}

A star semiring or closed semiring has the additional unary operator $-^*$,
defined as

\begin{equation*}
  a^* = 1 + a a^* = 1 + a^* a
\end{equation*}

\pause

Intuitively, $a^* = 1 + a + a^2 + a^3 + \ldots$.\footnote{This intuition is
not correct for all closed semirings} What would this look like for types?

\pause

For lists, we can have lists of no length or one length or ...

< data List a = [] | a | (a, a) | (a, a, a) | ...

\vpause

\acp{adt} can be described as a closed semiring.

\end{frame}


\section{Zippers}


\begin{frame}
\frametitle{Zippers are great for constant (locally) updating structures}

Zippers are a convenient way of traversing a structure, keeping track of
where you are.

Zippers are also sometimes known as a ``one hole context''

\pause

< data List = [] | a : (List a)

> data Zipper a = Zipper [a] [a]

\pause

Zippers allow for constant time \emph{local} updates to a data structure.

\pause

Zippers are used in extensively in XMonad.

\end{frame}


\section{Derivatives}

\begin{frame}
\frametitle{Derivatives can also lead to a one hole context}

The derivative of a type is often known as a ``one hole context''. We can
think of this as marking where we are in a data structure.

\end{frame}


\begin{frame}
\frametitle{Derivative of a Constant}

We know from calculus a few derivative rules. The basics are that the
derivative of a constant is zero.

\begin{equation*}
  \partial K = 0
\end{equation*}

\pause

How does this look for a value of type |K|?

< Diff Nil = Zero

A value has no context; it is the only thing in town!

\end{frame}


\begin{frame}
\frametitle{Derivative of a variable}

The derivative of variable by itself is 1.

\begin{equation*}
  \partial Id = 1
\end{equation*}

\pause

How does this look for a value of type |Id|?

< Diff ((IdD x))  =  ()

\end{frame}


\begin{frame}
\frametitle{Derivative of a Product}

The derivative of a product is described by the product rule\footnote{or
more generally by the Leibniz rule}.

\begin{equation*}
  \partial (f \times g) = \partial f \times g + f \times \partial g
\end{equation*}

\pause

If we take the algebraic form of |Prod x x|.

< Diff ((Prod x x)) = Sum (Prod One x) (Prod x One)

The values can be either |L ((ProdD OneD x))| or |R ((ProdD x OneD))|

\pause

This represents that we can either be in the left side of a product or the
right hand side.


\end{frame}



\begin{frame}
\frametitle{Derivative of a Sum}

The derivative of a sum is defined by the following rule.

\begin{equation*}
  \partial (f + g) = \partial f + \partial g
\end{equation*}

\pause

If we take the algebraic form of |Sum x One|.

< Diff ((Sum x One)) = Sum One Zero

%% We can think of this as the derivative of the |Maybe| type.
%%
%% < data Maybe         a   =  Nil | Just x     --  $\cong$ |Sum 1 x|
%% < data MaybeContext  ()  =  MaybeContext ()  --  Only one context

\end{frame}


\section{Dissection}

\begin{frame}
\frametitle{Dissection is a generalization of derivatives}

When we calculate the derivative of a type, we don't differentiate the
values to the left of the hole from the values to the right.

\pause

Take for example a |List|.

< Diff (List a) = (List a, List a) -- the same as Zipper

\pause

But for dissection, we note those values that have already been processed
differently from those yet to be seen.

\end{frame}


\begin{frame}
\frametitle{Dissection follows the same rules}

Dissection follows the same rules.

\begin{align*}
  \diss K  &= 0\\
  \diss Id &= 1\\
  \diss (p + q) &= \diss p + \diss q\\
  \diss (p \times q) &= \diss p \times \joker q + \clown p \times \diss q
\end{align*}

\pause

What is $\clown$ and $\joker$?

\pause

$\clown$ labels the values in the type as having been processed,

\pause

$\joker$ labels the values in the type as having \emph{yet} to be processed.

\end{frame}


\begin{frame}
\frametitle{Dissection of a List}

What if we want to dissect a |List|? Remember the derivative.

< Diff ((List a)) = (List a, List a)

\pause

It is the same as |Zipper| but where we annotate the parts of the list we
have already seen (|Clown ((List a))|) and those we have yet to see (|Joker
((List a))|).

< Diss ((List a)) = (Clown ((List a)), Joker ((List a)))

\pause

If we had used the same label in the dissection, we get the derivative.

< Diss ((List a)) = (Clown ((List a)), Clown ((List a))) `iso` ((List a), List a))

\end{frame}


\begin{frame}
\frametitle{Summary}

What have we learned today?

\begin{itemize}[<+->]
  \item How to describe an algebraic structure $(\sets, Op)$ and monoids.
  \item How |Sum| and |Prod| each form monoids, and together form a
        closed semiring.
  \item How to take the derivative of a type, giving a one hole context.
  \item How to take the dissection of a type, giving a one hole context
        parameterized with where you have been before.
\end{itemize}

\end{frame}


\appendix

\section{Data Types a la Carte}

\begin{frame}
\frametitle{Identity Data Type}

We can define an identity data type that only holds a single
value.\footnote{All definitions are originally from~\cite{clowns}}.

> data Id1           x  =  Id1 x                 -- element

%if False

> deriving instance Show x => Show (Id1 x)

%endif

We note that this can be a functor with the following definition.

> instance Functor Id1 where
>   fmap f (Id1 x) = Id1 (f x)

\end{frame}


\begin{frame}
\frametitle{Constant Data Type}

> data K1 a         x  =  K1 a                 -- constant

%if False

> deriving instance Show a => Show (K1 a x)

%endif

This is the same as the |const| function. |const x :: b -> a| is a function
that ignores its input.

We note that this can also be a functor.

> instance Functor (K1 a) where
>   fmap f (K1 a) = K1 a

\end{frame}


\begin{frame}
\frametitle{Pairs}

Data types can be defined by the product of two other types. Often this
appears as the following.

> data Pair a b = Pair a b

We can write this generically as follows.

> data (Prod1 p q)  x  =  P1 (p x) (q x)       -- pairing

%if False

> deriving instance (Show (p x), Show (q x)) => Show ((Prod1 p q) x)

%endif

And again we note that this can be a functor.

> instance (Functor p, Functor q) => Functor (Prod1 p q) where
>   fmap f (P1 p q) = P1 (fmap f p) (fmap f q)

\end{frame}


\begin{frame}
\frametitle{Sums}

We can also construct a data type that chooses between one of two alternates.

< data Either a b = Left a | Right b

We can write this generically as follows.

> data (Sum1 p q)   x  =  L1 (p x) | R1 (q x)  -- choice, aka Either

%if False

> deriving instance (Show (p x), Show (q x)) => Show ((Sum1 p q) x)

%endif

And again we note that this can be a functor.

> instance (Functor p, Functor q) => Functor (Sum1 p q) where
>   fmap f (L1 p) = L1 (fmap f p)
>   fmap f (R1 q) = R1 (fmap f q)

\end{frame}


\begin{frame}
\frametitle{One}

If we think hard, we can think of a type that can only be constructed in one
manner.\footnote{Remember that the only inhabitant of |Unit| is |Unit|. We
ignore |bottom|.}

> type Unit = ()

This can be encoded in our generic patterns using |K1|.

%format one = "1_{v}"

> type One1 = K1 ()
> one = K1 ()

\end{frame}


\begin{frame}
\frametitle{Zero?}

What if I want to define a type that cannot be inhabited at all? I would
want something like the following.

< data Zero = ???

Well we can make an uninhabited type in the following way.

%format Zero1 = "0_{1}"

> data Zero1

This means we can never call the following function, which is probably a
good thing. :-)




\end{frame}


\begin{frame}
\frametitle{Recreating Maybe}

Using this construction, we can recreate |Maybe|

> type Maybe a = ((Sum1 One1 Id1)) a
>
> nothing  =  L1 (K1 ())
> just x   =  R1 (Id1 x)

Constructing a value is pretty simple.

> val = just 5 :: Maybe Integer

And note that |Maybe| already has a definition of |fmap|!

> newVal = fmap (4 +) val -- $\equiv just\ 9$


\end{frame}


\begin{frame}
\frametitle{Back to Monoids}

Remember that a monoid $(\sets, Op)$ has to have a closed, associative
binary operator with an identity element.

A \emph{semiring} is when two monoids are defined over the same set, have
two different identity elements, and one of the monoids commutes |(+)|. We
write this as follows.

\begin{equation*}
  (\sets, +, 1, \cdot, 0)
\end{equation*}

\end{frame}


\begin{frame}
\frametitle{Product Types}

Remember that the |Prod1| type is defined as follows.

< data (Prod1 p q)  x  =  P1 (p x) (q x)       -- pairing

If we define the set of all Haskell types \hask, then

\end{frame}


\begin{frame}
\frametitle{List}
%format Mu (p) = "\mu\," p
%format In (p) = "\unmu\," p

%format nil = []
%format cons (a) (as) = "(:)\ " a "\ " as
%format `cons` = " : "

If we attempt to make a list with this formulation, then we could write the
following.

> type ListF  a   =  (Sum1 One1 (Prod1 (K1 a) Id1))

We would like a list that contained |ListF|, but if we try

< type List = ListF List

we get an infinite type. To fix this, we need to "tie the knot".

> data Mu p = In (p (Mu p))

And now we can define our list as follows.

> type List  a   =  Mu ((ListF a))
>
> nil        =  In ((L1 one))
> cons a as  =  In ((R1 (P1 (K1 a) (Id1 as))))

%if False

> instance (Show a) => Show (List a) where
>   show (In (L1 (K1 ()))) = "[]"
>   show (In (R1 (P1 (K1 a) (Id1 as)))) = show a ++ " : " ++ show as

> infixr 7 `cons`


%endif

\end{frame}


\end{document}
