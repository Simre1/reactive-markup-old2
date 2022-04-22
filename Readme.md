# Reactive Markup

Reactive markup is a haskell library for declaratively defining user interfaces. It is currently still in development phase, so drastic changes happen regularly. 

Some of the features of this library are:
  - Declarative components
  - UI as a function from model to components
  - Automatic updating of the UI on model changes
  - Contexts to constrain where components can be used
  - Customizable interpretation of components (currently, only a GTK backend is available)

This library tries to disallow all UI errors at compile time, so compiled code will definitely produce a working UI.  


Here is a small code example and an image of the corresponding GTK UI:
```
import ReactiveMarkup.Target.Gtk
import ReactiveMarkup
import Data.Void


main :: IO ()
main = do
  runGtk app

renderGUI :: Markup Gtk Root Void
renderGUI = bold "Hello Reactive Markup"

app :: App Gtk EmptyF Void
app =
  App
    { appRender = \_ -> renderGUI,
      appHandleEvent = absurd,
      appInitialState = EmptyF,
      appName = "Hello Reactive Markup"
    }
```

![Gtk Hello Reactive-Markup example](hello.png)

## How to build yourself

First and foremost, you need to have GTK 4 installed! Then the following should do the trick:
```
git clone https://github.com/Simre1/reactive-markup.git
cd reactive-markup
cabal build all
```

This will build the `reactive-markup` library, the `reactive-markup-gtk` backend as well as the GTK examples. *Initial* compilation will take quite a while due to the GTK dependencies.

## Use as a library

To use Reactive Markup as a library, you will have to clone the git repository and add the reactive-markup folder manually to your build environment. Otherwise, it can be used like any other haskell library.

## Quick Tutorial

### UI components

You can define your UI by builing up the `Markup` type using the available components like `text`, `button`, `column` and so on. For example, to create a list consisting of some text and a button:
```haskell
textAndButton :: Markup Gtk Block Void
textAndButton = column 
  [ italic "Some text",
    button "Click me"
  ]
```

You can use functions and let-expressions to factor out code and make reusable UI components.
```haskell
textAndButton :: Markup Gtk Block Void
textAndButton = 
  let boldText = bold "Bold text"
  in column 
    [ boldText,
      italic boldText,
      button "Click me"
    ]
```

### Reacting to changes

Assuming that you create your UI as a function from model state to components, then the UI will automatically update itself on model state changes. However, some boilerplate is needed.

Here is an example:
```haskell
searchComponent :: Dynamic Gtk Bool -> Markup Gtk Block Void
searchComponent isBool = dynamicMarkup isBool $ \actualIsBool -> row 
  [if actualIsBool then "Active" else "Inactive"]
```

The `Dynamic` part means that the `Bool` value may change. To actually get at the `Bool` value, you need to use `dynamicMarkup` which gives you access to the `Bool` value in the function argument `actualIsBool`. Whenever the `Bool` value changes, `dynamicMarkup` will use the given function to determine the new UI.

### Handling Events

Components can spawn events which are then passed upwards implicitly through the component hierarchy.

Here is a button which emits an event of type `Text` and the value "Event message":
```haskell
buttonWithTextEvent :: Markup Gtk Block Text
buttonWithTextEvent = button "Click" (#click ?= "Event message")
```

Take note that the event type is part of the `Markup` type. This means that by looking at the type we can determine the type of the events that a component can spawn. `buttonWithTextEvent` spawns events of type `Text`.  

If we use `buttonWithTextEvent` within another component, then the resuling component also spawns events of type `Text`.

```haskell
columnWithTextEvent :: Markup Gtk Block Text
columnWithTextEvent = column [ buttonWithTextEvent ]
```

### First dynamic behavior

You cannot directly create `Dynamic` values and you cannot directly interact with events either. However, there are components which you can use to do so.

For example `simpleLocalState`:

```haskell
searchComponent :: Markup Gtk Block Void
searchComponent = simpleLocalState handleButtonClick initialState buttonWithNumber
  where
    initialState :: Int
    initialState = 0

    handleButtonClick :: () -> LocalUpdate Int Void -> LocalUpdate Int Void
    handleButtonClick () (LocalUpdate state _) = LocalUpdate (state + 1) Nothing
    
    buttonWithNumber :: Dynamic Gtk Int -> Markup Gtk Block ()
    buttonWithNumber int = dynamicMarkup int $ \i -> button (show i) (#click ?~ ())
```

`simpleLocalState` allows you to have some local state for a component and update it based on events happening within that component.