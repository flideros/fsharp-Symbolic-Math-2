(*
This control acts as the main entry point to the lab. It is currently based on 
a tab control so the lab can host any arbitrary WPF content. The Test Canvas tab
is where my current development code resides, so it may not be fully functional
as I update my code often. The Past Projects tab will launch finished projects.
If you clone this repository, feel free to add tabs or use the individual projects
directly in your custom environment.

I prefer to code in F# so Math Lab UI is just the bare bones C# WPF app to launch 
the window.
*)

namespace MathLab 

open System.Windows          
open System.Windows.Controls  
open System.Windows.Media  

type Lab() as lab =     
    inherit UserControl()

    // Controls
    let testCanvas = BasicCalculator.Calculator()    
    
    let pastProjects = 
        let sp = StackPanel()   
        let basicCalculator = 
            let b = Button(Content = "Basic Calculator")
            let handleClick () =
                let w = Window(SizeToContent = SizeToContent.WidthAndHeight)
                do  w.Content <- BasicCalculator.Calculator()
                w.Topmost <- true
                w.Show() 
            b.Click.AddHandler(RoutedEventHandler(fun _ _ -> handleClick()))
            b     
        let colorPicker = 
            let b = Button(Content = "Color Picker")
            let handleClick () =
                let w = Window(SizeToContent = SizeToContent.WidthAndHeight)
                do  w.Content <- ControlLibrary.HsvColorPicker(selectedColor=ControlLibrary.SharedValue(Colors.Transparent))
                w.Topmost <- true
                w.Show() 
            b.Click.AddHandler(RoutedEventHandler(fun _ _ -> handleClick()))
            b     
        do  sp.Children.Add(basicCalculator) |> ignore
            sp.Children.Add(colorPicker) |> ignore
        sp

    // Tab Control 
    let tabs = TabControl()

    let item0 = TabItem(Header = "Test Canvas")
    do  item0.Content <- testCanvas
    let item1 = TabItem(Header = "Past Projects")
    do  item1.Content <- pastProjects

    do  tabs.Items.Add(item0) |> ignore
        tabs.Items.Add(item1) |> ignore
        lab.Content <- tabs
 