﻿namespace Math.Presentation

open MathML
open System
open System.Windows       
open System.Windows.Controls  
open System.Windows.Shapes  
open System.Windows.Media
open MathematicalAlphanumericSymbols
open ControlLibrary

type Position = {x:float;y:float}
type Size = {scaleX :float; scaleY :float}
type Font = 
    {emSquare : float<MathML.em>
     typeFace : Typeface
     size : float<MathML.px>               
    }
type Glyph = 
    {path:Path;
     overHangBefore:float;
     extent:float;
     baseline:float;
     overHangAfter:float;
     height:float;
     leftBearing:float;
     rightBearing:float;
     width:float;
     string:string;
     font:Font;
     lSpace:float;
     rSpace:float
     element:MathMLElement
     }
type GlyphRow = 
    {grid:Grid;
     rowWidth:float;
     rowHeight:float;
     leftBearing:float;
     rightBearing:float;
     leftElement:MathMLElement;
     rightElement:MathMLElement;
     overHangAfter:float
     }
type GlyphBuilder = Font -> Element -> Glyph 
type GlyphBox (glyph) as glyphBox =
    inherit Border(BorderThickness=Thickness(1.5),BorderBrush=Brushes.Red)
    let g = Grid()
    
    let mLine = 
        let p = Path(Stroke = Brushes.Cyan, StrokeThickness = 4.)

        let pf = PathFigure(StartPoint = Point(0., glyph.baseline - (MathPositioningConstants.axisHeight * (glyph.font.emSquare / 1000.<MathML.em>))))        
        do  pf.Segments.Add( LineSegment( Point(glyph.width, glyph.baseline - (MathPositioningConstants.axisHeight * (glyph.font.emSquare / 1000.<MathML.em>))), true ))
        let pg = PathGeometry() 
        do  pg.Figures.Add(pf)
            p.Data <- pg
            p.SetValue(Grid.ColumnSpanProperty,3)
        p
    let bLine = 
        let p = Path(Stroke = Brushes.White, StrokeThickness = 4.)
        let pf = PathFigure(StartPoint = Point(0., glyph.baseline))        
        do  pf.Segments.Add( LineSegment( Point(glyph.width, glyph.baseline), true ))
        let pg = PathGeometry() 
        do  pg.Figures.Add(pf)
            p.Data <- pg
            p.SetValue(Grid.ColumnSpanProperty,3)
        p
    
    let column0 = 
        match glyph.leftBearing < 0. with
        | true -> ColumnDefinition(Width = GridLength(glyph.width))
        | false -> ColumnDefinition(Width = GridLength.Auto)    
    let column1 = 
        match glyph.rightBearing < 0. with
        | true -> ColumnDefinition(Width =  GridLength.Auto)
        | false -> ColumnDefinition(Width = GridLength(glyph.rightBearing))
      
    do  glyph.path.SetValue(Grid.ColumnProperty,0)
        glyph.path.SetValue(Grid.ColumnSpanProperty,2)
        g.ColumnDefinitions.Add(column0)
        g.ColumnDefinitions.Add(column1)
    
    let row0 = 
        RowDefinition(
            Height = GridLength(match glyph.overHangBefore > 0. with 
                                | false -> 0.
                                | true -> glyph.overHangBefore))
    let row1 = RowDefinition(Height = GridLength.Auto)
           
    do  glyph.path.SetValue(Grid.RowProperty,1)
        mLine.SetValue(Grid.RowProperty,1)
        bLine.SetValue(Grid.RowProperty,1)
        g.RowDefinitions.Add(row0)
        g.RowDefinitions.Add(row1)
        
        g.Children.Add(glyph.path) |> ignore
        g.Children.Add(mLine) |> ignore
        g.Children.Add(bLine) |> ignore         
           
    do  //glyph.path.
        glyphBox.SetValue(Grid.RowProperty,1)
        glyphBox.SetValue(Grid.RowSpanProperty,3)
        glyphBox.Child <- g
type TypeObject =
    | Glyph of Glyph
    | GlyphRow of GlyphRow

module TypeSetting = 
    open MathML
    // Constants
    let basisSize = 100.<px>
    let basisEmSquare = 10.<em>
    let textSizeScaleFactor = float (basisEmSquare / basisSize)
    let textBaseline = 762.

    //  Font Sizes
    let textSizeFont = {emSquare = 1000.<em>; typeFace = Text.STIX2Math_Typeface; size = basisSize}
    let scriptSizeFont = {emSquare = 700.<em>; typeFace = Text.STIX2Math_Typeface; size = basisSize}
    let scriptScriptSizeFont = {emSquare = 500.<em>; typeFace = Text.STIX2Math_Typeface; size = basisSize}
        
    //  Format Text
    let formatTextWithFont = fun t font -> Text.format t font.typeFace font.emSquare    
    
    //  Transfomations
    let scaleGlyphBox (glyphBox :GlyphBox) (s:Size) = 
        do glyphBox.RenderTransform <- ScaleTransform(ScaleX = s.scaleX,ScaleY = s.scaleY)
    let placeGlyphBox (glyphBox :GlyphBox) (p:Position) = 
        do glyphBox.RenderTransform <- TranslateTransform(X = p.x, Y = p.y)
    let transformGlyphBox (glyphBox :GlyphBox) (p:Position) (s:Size) = 
        let tranforms = TransformGroup()
        do  tranforms.Children.Add(TranslateTransform(X = p.x, Y = p.y))
            tranforms.Children.Add(ScaleTransform(ScaleX = s.scaleX,ScaleY = s.scaleY))            
            glyphBox.RenderTransform <- tranforms
    
    //  Getters
    let getHorizontalKern leftGlyph rightGlyph =
        let typeSetPair = formatTextWithFont (leftGlyph.string + rightGlyph.string) leftGlyph.font        
        typeSetPair.Width - leftGlyph.width - rightGlyph.width
    let getOperatorString (operator : Operator) = 
        let rec loop oc =
            match oc with
            | Unicode u -> (char u).ToString()
            | Char c -> c.ToString()
            | UnicodeArray a -> 
                let chars = Array.map (fun oc -> (char)(loop oc)) a
                new string (chars)
        loop operator.character
    let getStringAtUnicode u = (char u).ToString()   
    let getFontFromTokenElement (el:Element) = 
        let mathSize = 
            List.tryFind (fun x -> 
                match x with
                | MathSize _ -> true 
                | _ -> false) el.attributes
        match mathSize with 
        | Some (MathSize (EM s)) -> {emSquare = s * 1000.; typeFace = Text.STIX2Math_Typeface; size = basisSize}
        | _ -> textSizeFont
    let getGridFromTypeObject t = match t with | GlyphRow gr -> gr.grid | Glyph g -> new Grid() 
    let getWidthFromTypeObject t = match t with | GlyphRow gr -> gr.rowWidth | Glyph g -> g.width
    let getHeightFromTypeObject t = match t with | GlyphRow gr -> gr.rowHeight | Glyph g -> g.height    
    let getLeftElementFromTypeObject t = match t with | GlyphRow gr -> gr.leftElement | Glyph g -> g.element
    let getRightElementFromTypeObject t = match t with | GlyphRow gr -> gr.rightElement | Glyph g -> g.element

    // Builders
    let makeGlyphBox glyph (p:Position) = 
            let x,y = p.x, p.y
            let y' = y - (match glyph.overHangBefore > 0. with | true -> glyph.overHangBefore | false -> 0.)
            let gb = GlyphBox(glyph)
            do  gb.Width <- gb.Width * (glyph.font.size / 960.<px>)
                gb.Height <- gb.Height * (glyph.font.size / 960.<px>)
            do  gb.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> 
                            transformGlyphBox 
                                gb // glyph box
                                {x = x; y = y'} // position
                                {scaleX = glyph.font.size / 960.<px>; scaleY = glyph.font.size / 960.<px>} )) // font size          
            gb
    let makeGlyph :GlyphBuilder = 
        fun font el ->
            let symbol = 
                match el.operator with 
                | Some o -> getOperatorString o
                | Option.None -> el.symbol
            let lSpace = 
                let space = 
                    List.tryFind (fun x -> 
                        match x with
                        | LSpace _ -> true 
                        | _ -> false) el.attributes
                match space with 
                | Some (LSpace s) -> Operator.getValueFromLength font.emSquare s
                | _ -> 0.
            let rSpace = 
                let space = 
                    List.tryFind (fun x -> 
                        match x with
                        | RSpace _ -> true 
                        | _ -> false) el.attributes
                match space with 
                | Some (RSpace s) -> Operator.getValueFromLength font.emSquare s
                | _ -> match el.element = (Token Mo) with
                        | true -> (Operator.getValueFromLength font.emSquare (NamedLength ThickMathSpace))
                        | false -> 0.
                
            let text = 
                let variant = 
                    List.tryFind (fun x -> 
                        match x with
                        | MathVariant _ -> true 
                        | _ -> false) el.attributes
                match variant with
                | Some (MathVariant v) -> v 
                | _ -> Normal
                |> MathematicalAlphanumericSymbolMap.stringToVariant symbol
            let formattedText = Text.format text font.typeFace font.emSquare

            let mathColor = 
                let color = 
                    List.tryFind (fun x -> match x with
                                            | MathColor _ -> true 
                                            | _ -> false) el.attributes
                match color with
                | Some (MathColor m) -> m 
                | _ -> Brushes.Black :> Brush
            let p = Path(Stroke = mathColor, Fill = mathColor)            
                
            let geometry = 
                let drawingGroup = DrawingGroup()
                let drawingContext = drawingGroup.Open()
                let textLine = Text.format text font.typeFace font.emSquare
                do  textLine.Draw(drawingContext,Point(0.,0.),TextFormatting.InvertAxes.None)
                    drawingContext.Close()             
                drawingGroup
            let rec draw (dg :DrawingGroup) = 
                let items = dg.Children.Count
                let gg = GeometryGroup()
                let rec getDrawing i = 
                    match i > 0 with
                    | false -> gg
                    | true -> 
                        match dg.Children.Item(i-1) with
                        | :? GeometryDrawing as gd -> 
                            //do  gg.Children.Add(gd.Geometry)
                            getDrawing (i-1)
                        | :? GlyphRunDrawing as gr -> 
                            do  gg.Children.Add(gr.GlyphRun.BuildGeometry())
                            getDrawing (i-1)
                        | :? DrawingGroup as dg -> 
                            do  gg.Children.Add(draw dg)
                            getDrawing (i-1)
                        | _ -> gg
                getDrawing items            
                
            do  p.Data <- (draw geometry).GetFlattenedPathGeometry()
            {path=p;
             leftBearing = formattedText.OverhangLeading; 
             extent = formattedText.Extent;
             overHangAfter = formattedText.OverhangAfter;
             overHangBefore = formattedText.Extent - formattedText.OverhangAfter - formattedText.Height;
             rightBearing = formattedText.OverhangTrailing; 
             baseline = formattedText.Baseline; 
             width = formattedText.Width; 
             height = formattedText.Height;
             font = font;
             string = text;
             lSpace = lSpace;
             rSpace = rSpace
             element = el.element
             }
    
    let makeRowFromGlyphs (glyphs:TypeObject list) =
        let errorGlyph = makeGlyph textSizeFont (Element.build (GeneralLayout Merror) [] [] "\ufffd" Option.None)
        let glyphs = List.map (fun x -> match x with | Glyph g -> g | GlyphRow gr -> errorGlyph ) glyphs  
        let overHangAfter = 
            List.fold (
                fun acc (g:Glyph) -> 
                    match acc < g.overHangAfter && g.overHangAfter <= 0. with 
                    | true -> acc 
                    | false -> g.overHangAfter) 0. glyphs
        let g = Grid()
        let row0 = RowDefinition(Height = GridLength.Auto)
        let row1 = RowDefinition(Height = GridLength.Auto)
        do  g.RowDefinitions.Add(row0)
            g.RowDefinitions.Add(row1)
            
        let kerns = 
        // compare adjacent glyph widths to the typeset width of the pair.
            let leftGlyphs = List.truncate (glyphs.Length-1) glyphs
            let rightGlyphs = glyphs.Tail
            let glyphPairs = List.zip leftGlyphs rightGlyphs
            let kerns = List.scan (fun acc (l,r) -> acc + (getHorizontalKern l r)) 0. glyphPairs
            kerns        
        let mathSpaces = 
            // Apply operator lSpace and rSpace.
            let leftGlyphs = List.truncate (glyphs.Length-1) glyphs
            let rightGlyphs = glyphs.Tail
            let glyphPairs = List.zip leftGlyphs rightGlyphs
            let spaces = List.scan (fun acc (l,r) -> acc + l.rSpace + r.lSpace - l.rightBearing - r.leftBearing) 0. glyphPairs
            // Specify space based on em size
            match glyphs.Head.font.emSquare = 1000.<em> with
            | true -> spaces
            | false -> List.map (fun x -> 0. * x) spaces // In this case, do not apply to script sizes
        let positions = 
            let initialPosition = {x=0.;y=0.}
            // positions = 
            let p = List.scan (fun acc (x : Glyph) -> {x = (acc.x + x.width); y = 0.}) initialPosition glyphs
            List.truncate glyphs.Length p  
            // apply kerns
            |> List.mapi (fun i (p:Position) -> {x = p.x + kerns.[i]; y = p.y})
            // apply math spacees
            |> List.mapi (fun i (p:Position) -> {x = p.x + mathSpaces.[i]; y = p.y})
        let leftBearing = 
            match glyphs.Head.lSpace = 0. with
            | false -> glyphs.Head.lSpace + glyphs.Head.leftBearing
            | true -> glyphs.Head.leftBearing
        let rightBearing = 
            match (List.rev glyphs).Head.rSpace = 0. with
            | false -> (List.rev glyphs).Head.rSpace //+ (List.rev glyphs).Head.rightBearing 
            | true -> (List.rev glyphs).Head.rightBearing
        let width = 
             List.fold (fun acc x -> x.width + acc) 0. glyphs + // glyphWidths
             List.fold (fun acc x -> x + acc) 0. kerns        + // kerns
             List.fold (fun acc x -> x + acc) 0. mathSpaces     // math space
        let height = (List.maxBy (fun x -> x.height) glyphs).height
        let glyphBoxes = List.map2 (fun g p -> makeGlyphBox g p) glyphs positions

        do  List.iter (fun x -> g.Children.Add(x) |> ignore) glyphBoxes
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = glyphs.Head.element;
         rightElement = (List.rev glyphs).Head.element;
         overHangAfter = overHangAfter
         } |> GlyphRow    
    let makeRowFromTypeObjects (typeObjects:TypeObject list) =
        let g = Grid()
        let row0 = RowDefinition(Height = GridLength.Auto)
        let row1 = RowDefinition(Height = GridLength.Auto)
        do  g.RowDefinitions.Add(row0)
            g.RowDefinitions.Add(row1)
               
        let rec getRows (typeObjects:TypeObject list) acc = 
            match typeObjects with
            | [] -> acc
            | GlyphRow gr :: tail -> getRows tail (List.concat [acc; [GlyphRow gr]])
            | Glyph g :: tail -> 
                let tail' = Seq.toList(Seq.skipWhile (fun x -> match x with | Glyph _ -> true | GlyphRow _ -> false) tail)
                let r0 = 
                    let r = Seq.toList(Seq.takeWhile (fun x -> match x with | Glyph _ -> true | GlyphRow _ -> false) tail)
                    makeRowFromGlyphs ((Glyph g)::r)
                getRows tail' (List.concat [acc; [r0]])
        let rows = 
            getRows typeObjects [] 
            |> List.map (fun x -> 
                match x with 
                | GlyphRow gr -> gr 
                | Glyph g -> 
                    {grid=Grid();
                     rowWidth=0.;
                     rowHeight=0.;
                     leftBearing=0.;
                     rightBearing=0.
                     leftElement = g.element;
                     rightElement = g.element;
                     overHangAfter=0.})
        let positions = 
            let initialPosition = {x = 0.;y = 0.}
            let p = 
                List.scan (fun acc x -> 
                    {x = (acc.x + (x.rowWidth + x.rightBearing) / 10.); y = 0.}) initialPosition rows            
            List.truncate rows.Length p
        let mappedRows = 
            List.map2 (fun gr p -> 
                let gOut = gr.grid
                do  gOut.RenderTransform <- TranslateTransform(p.x + (gr.leftBearing/10.),p.y)
                gOut) rows positions

        let leftBearing = rows.Head.leftBearing
        let rightBearing = (List.rev rows).Head.rightBearing 
        let width = List.fold (fun acc x ->  x.rowWidth + acc) 0. rows
        let height = (List.maxBy (fun x -> x.rowHeight) rows).rowHeight
        let leftElement, rightElement =
            (getLeftElementFromTypeObject typeObjects.Head), 
            (getRightElementFromTypeObject (List.rev typeObjects).Head)
        let overHangAfter = 
            List.fold (
                fun acc g -> 
                    match g with 
                    | Glyph g -> 
                        match acc < g.overHangAfter && g.overHangAfter <= 0. with 
                        | true -> acc 
                        | false -> g.overHangAfter
                    | GlyphRow g -> 
                        match acc < g.overHangAfter && g.overHangAfter <= 0. with 
                        | true -> acc 
                        | false -> g.overHangAfter) 0. typeObjects

        do  List.iter (fun x -> g.Children.Add(x) |> ignore) mappedRows
        
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = leftElement;
         rightElement = rightElement;
         overHangAfter = overHangAfter
         }|> GlyphRow
    
    let makeSuperScriptFromTypeObjects 
        (target:TypeObject) 
        (script:TypeObject) 
         superscriptShiftUp =        
        
        let g = Grid()

        let targetGrid =
            match target with
            | GlyphRow gr -> gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = makeGlyphBox gl {x=0.;y=0.}
                do grid.Children.Add(gb) |> ignore
                grid
        let targetRBearing =
            match target with
            | GlyphRow gr -> gr.rightBearing
            | Glyph gl -> gl.rightBearing + gl.rSpace   
        let targetLBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength MediumMathSpace)) * (1000./960.)          
        let scriptGrid =             
            let position = 
                let x = (getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = (-superscriptShiftUp) * textSizeScaleFactor 
                {x = x; y = y}
            
            match script with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g        
        let leftBearing = targetLBearing
        let rightBearing = MathPositioningConstants.spaceAfterScript
        let width = (getWidthFromTypeObject target) + (getWidthFromTypeObject script)
        let height = (getHeightFromTypeObject target) + ((-superscriptShiftUp) * textSizeScaleFactor)
        
        do  List.iter (fun x -> g.Children.Add(x) |> ignore) [targetGrid; scriptGrid]
        
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = Script Msup;
         rightElement = Script Msup;
         overHangAfter = 0.         
         }|> GlyphRow
    
    let makeSubScriptFromTypeObjects
        (target:TypeObject)
        (script:TypeObject)
         subscriptShiftDown =        
    
        let g = Grid()

        let targetGrid =
            match target with
            | GlyphRow gr -> gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = makeGlyphBox gl {x=0.;y=0.}
                do grid.Children.Add(gb) |> ignore
                grid
        let targetRBearing =
            match target with
            | GlyphRow gr -> gr.rightBearing
            | Glyph gl -> gl.rightBearing + gl.rSpace
        let targetLBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength MediumMathSpace)) * (1000./960.)
        let scriptGrid =             
            let position = 
                let x = (getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = subscriptShiftDown * textSizeScaleFactor 
                {x = x; y = y}
        
            match script with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g    
        let leftBearing = targetLBearing
        let rightBearing = MathPositioningConstants.spaceAfterScript  
        let width = (getWidthFromTypeObject target) + (getWidthFromTypeObject script)
        let height = (getHeightFromTypeObject target) + (subscriptShiftDown * textSizeScaleFactor)
    
        do  List.iter (fun x -> g.Children.Add(x) |> ignore) [targetGrid; scriptGrid]
    
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = Script Msub;
         rightElement = Script Msub;
         overHangAfter = 0.         
         }|> GlyphRow

    let makeSuperSubScriptFromTypeObjects
        (target:TypeObject) 
        (superScript:TypeObject)
         superscriptShiftUp
        (subScript:TypeObject)
         subscriptShiftDown  =        
    
        let g = Grid()

        let targetGrid =
            match target with
            | GlyphRow gr -> gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = makeGlyphBox gl {x=0.;y=0.}
                do grid.Children.Add(gb) |> ignore
                grid
        let targetRBearing =
            match target with
            | GlyphRow gr -> gr.rightBearing
            | Glyph gl -> gl.rightBearing + gl.rSpace
        let targetLBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength MediumMathSpace)) * (1000./960.)
        let superScriptGrid =             
            let position = 
                let x = (getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = (-superscriptShiftUp) * textSizeScaleFactor 
                {x = x; y = y}
            
            match superScript with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
        let subScriptGrid =             
            let position = 
                let x = (getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = subscriptShiftDown * textSizeScaleFactor 
                {x = x; y = y}
    
            match subScript with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
    
        let leftBearing = targetLBearing
        let rightBearing = MathPositioningConstants.spaceAfterScript  
        let width = 
            let widerScript = 
                match (getWidthFromTypeObject superScript) > (getWidthFromTypeObject subScript) with
                | true -> (getWidthFromTypeObject subScript)
                | false -> (getWidthFromTypeObject subScript)
            (getWidthFromTypeObject target) + widerScript
        let height = (getHeightFromTypeObject target) + (subscriptShiftDown * textSizeScaleFactor)
    
        do  List.iter (fun x -> g.Children.Add(x) |> ignore) [targetGrid; superScriptGrid; subScriptGrid]
    
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = Script Msubsup;
         rightElement = Script Msubsup;
         overHangAfter = 0.         
         }|> GlyphRow
    
    let makeOverScriptFromTypeObjects
        (target:TypeObject) 
        (script:TypeObject) 
         overscriptShiftUp 
         overscriptDisplay =        
        
        let g = Grid()

        let targetGrid =
            match target with
            | GlyphRow gr -> gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = makeGlyphBox gl {x=0.;y=0.}
                do grid.Children.Add(gb) |> ignore
                grid        
        let targetRBearing =
            match target with
            | GlyphRow gr -> gr.rightBearing
            | Glyph gl -> gl.rightBearing + gl.rSpace
        let targetLBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength MediumMathSpace)) * (1000./960.)
        let targetWidth =
            match target with
            | GlyphRow gr -> gr.rowWidth
            | Glyph gl -> gl.width
        let scriptWidth =
            match script with
            | GlyphRow gr -> gr.rowWidth
            | Glyph gl -> gl.width        
        let scriptGrid =             
            let position = 
                let x = 
                    match overscriptDisplay with
                    | Block -> (targetWidth * textSizeScaleFactor) - (scriptWidth * textSizeScaleFactor) 
                    | Inline -> (MathPositioningConstants.upperLimitGapMin + getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = (-overscriptShiftUp) * textSizeScaleFactor 
                {x = x; y = y}
            
            match script with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g        
        let leftBearing = targetLBearing
        let rightBearing = 
            match overscriptDisplay with
            | Block -> MathPositioningConstants.mathLeading
            | Inline -> targetRBearing
        let width = 
            match overscriptDisplay with
            | Block -> (getWidthFromTypeObject target)
            | Inline -> (getWidthFromTypeObject target) + (getWidthFromTypeObject script)
        let height = (getHeightFromTypeObject target) + ((-overscriptShiftUp) * textSizeScaleFactor)
        do  List.iter (fun x -> g.Children.Add(x) |> ignore) [targetGrid; scriptGrid]
        
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = Script Mover;
         rightElement = Script Mover;
         overHangAfter = 0.         
         }|> GlyphRow
    
    let makeUnderScriptFromTypeObjects
        (target:TypeObject) 
        (script:TypeObject) 
         underscriptShiftDown 
         underscriptDisplay =        
    
        let g = Grid()

        let targetGrid =
            match target with
            | GlyphRow gr -> gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = makeGlyphBox gl {x=0.;y=0.}
                do grid.Children.Add(gb) |> ignore
                grid        
        let targetRBearing =
            match target with
            | GlyphRow gr -> gr.rightBearing
            | Glyph gl -> gl.rightBearing + gl.rSpace
        let targetLBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength MediumMathSpace)) * (1000./960.)
        let targetWidth =
            match target with
            | GlyphRow gr -> gr.rowWidth
            | Glyph gl -> gl.width
        let scriptWidth =
            match script with
            | GlyphRow gr -> gr.rowWidth
            | Glyph gl -> gl.width
        let scriptGrid =             
            let position = 
                let x = 
                    match underscriptDisplay with
                    | Block -> (targetWidth * textSizeScaleFactor) - (scriptWidth * textSizeScaleFactor)
                    | Inline -> (MathPositioningConstants.lowerLimitGapMin + getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = underscriptShiftDown * textSizeScaleFactor 
                {x = x; y = y}        
            match script with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g    
        let leftBearing = targetLBearing
        let rightBearing = 
            match underscriptDisplay with
            | Block -> MathPositioningConstants.mathLeading
            | Inline -> targetRBearing
        let width = 
            match underscriptDisplay with
            | Block -> (getWidthFromTypeObject target)
            | Inline -> (getWidthFromTypeObject target) + (getWidthFromTypeObject script)
        let height = (getHeightFromTypeObject target) + (underscriptShiftDown * textSizeScaleFactor)
        do  List.iter (fun x -> g.Children.Add(x) |> ignore) [targetGrid; scriptGrid]
    
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = Script Munde;
         rightElement = Script Munde;
         overHangAfter = 0.         
         }|> GlyphRow

    let makeUnderOverScriptFromTypeObjects
        (target:TypeObject) 
        (underScript:TypeObject)
         underscriptShiftDown
        (overScript:TypeObject)
         overscriptShiftUp 
         underoverscriptDisplay =        
    
        let g = Grid()

        let targetGrid =
            match target with
            | GlyphRow gr -> gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = makeGlyphBox gl {x=0.;y=0.}
                do grid.Children.Add(gb) |> ignore
                grid        
        let targetRBearing =
            match target with
            | GlyphRow gr -> gr.rightBearing
            | Glyph gl -> gl.rightBearing + gl.rSpace
        let targetLBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength MediumMathSpace)) * (1000./960.)
        let targetWidth =
            match target with
            | GlyphRow gr -> gr.rowWidth
            | Glyph gl -> gl.width
        let underscriptWidth =
            match underScript with
            | GlyphRow gr -> gr.rowWidth
            | Glyph gl -> gl.width
        let overscriptWidth =
            match overScript with
            | GlyphRow gr -> gr.rowWidth
            | Glyph gl -> gl.width

        let overScriptGrid =             
            let position = 
                let x = 
                    match underoverscriptDisplay with
                    | Block -> (targetWidth * textSizeScaleFactor) - (overscriptWidth * textSizeScaleFactor) 
                    | Inline -> (MathPositioningConstants.upperLimitGapMin + getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = -overscriptShiftUp * textSizeScaleFactor 
                {x = x; y = y}
            
            match overScript with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
        let underScriptGrid =             
            let position = 
                let x = 
                    match underoverscriptDisplay with
                    | Block -> (targetWidth * textSizeScaleFactor) - (underscriptWidth * textSizeScaleFactor)
                    | Inline -> (MathPositioningConstants.lowerLimitGapMin + getWidthFromTypeObject target - targetRBearing) * textSizeScaleFactor
                let y = underscriptShiftDown * textSizeScaleFactor 
                {x = x; y = y}
    
            match underScript with
            | GlyphRow gr -> 
                let g = gr.grid
                do g.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
            | Glyph gl -> 
                let g = Grid()
                let gb = makeGlyphBox gl position
                do  g.Children.Add(gb) |> ignore
                    g.Loaded.AddHandler(
                        RoutedEventHandler(
                            fun _ _ -> g.RenderTransform <- TranslateTransform(X = position.x, Y = position.y)))
                g
    
        let leftBearing = targetLBearing
        let rightBearing = 
            match underoverscriptDisplay with
            | Block -> MathPositioningConstants.mathLeading
            | Inline -> targetRBearing
        let width =             
            let scriptWidth = 
                match underscriptWidth >= overscriptWidth with
                | true -> underscriptWidth
                | false -> overscriptWidth
            match underoverscriptDisplay with
            | Block -> (getWidthFromTypeObject target)
            | Inline -> (getWidthFromTypeObject target) + scriptWidth
             
        let height = (getHeightFromTypeObject target) + (underscriptShiftDown * textSizeScaleFactor) + (overscriptShiftUp * textSizeScaleFactor)
    
        do  List.iter (fun x -> g.Children.Add(x) |> ignore) [targetGrid; overScriptGrid; underScriptGrid]
    
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = Script Munderover;
         rightElement = Script Munderover;
         overHangAfter = 0.         
         }|> GlyphRow    

    let makeFractionFromTypeObjects 
        (numerator:TypeObject) 
        (denominator:TypeObject)     
        (lineThickness:Length)  
        (numAlign:_NumAlign) 
        (denomAlign:_DenomAlign) 
        (bevelled:bool)
        (display : _Display) = 
        
        let numOverHangAfter = 
            match numerator with
            | GlyphRow gr -> gr.overHangAfter
            | Glyph g -> g.overHangAfter
        let fractionWidth = 
            let n,d = (getWidthFromTypeObject numerator), (getWidthFromTypeObject denominator)           
            match n > d, bevelled with
            | true, false -> n / 10.
            | false, false-> d / 10. 
            | _, true-> (d + n + MathPositioningConstants.skewedFractionHorizontalGap) / 10.
        let numeratorHorozontalShift = 
            match numAlign, bevelled with 
            | _NumAlign.Center, false -> 
                match (getWidthFromTypeObject numerator) * 0.1 >= fractionWidth with
                | true -> 0.
                | false -> ((getWidthFromTypeObject denominator) - (getWidthFromTypeObject numerator)) * 0.5
            | _NumAlign.Left, false -> 0.
            | _NumAlign.Right, false -> 
                match (getWidthFromTypeObject denominator) * 0.1 >= fractionWidth with
                | false -> 0.
                | true -> ((getWidthFromTypeObject denominator) - (getWidthFromTypeObject numerator))
            | _ , true -> 0.
        let denominatorHorozontalShift = 
            match denomAlign, bevelled with
            | _DenomAlign.Center, false -> 
                match (getWidthFromTypeObject denominator) * 0.1 >= fractionWidth with
                | true -> 0.
                | false -> ((getWidthFromTypeObject numerator) - (getWidthFromTypeObject denominator)) * 0.5
            | _DenomAlign.Left, false -> 0.
            | _DenomAlign.Right, false -> 
                match (getWidthFromTypeObject numerator) * 0.1 >= fractionWidth with
                | false -> 0.
                | true -> ((getWidthFromTypeObject numerator) - (getWidthFromTypeObject denominator))
            | _ , true ->  MathPositioningConstants.skewedFractionHorizontalGap + (getWidthFromTypeObject numerator)        
        let mathLine = (MathPositioningConstants.mathLeading + textBaseline - MathPositioningConstants.axisHeight) * textSizeScaleFactor
        let mathAxisCorrectionHeight = MathPositioningConstants.axisHeight * (MathPositioningConstants.scriptPercentScaleDown / 100.) * (1000./960.)
        
        let mLine = 
            let thickness = (Operator.getValueFromLength 306.<em> lineThickness) * (100./960.)
            let p = Path(Stroke = Brushes.Black, StrokeThickness = thickness)
            let pf = PathFigure(StartPoint = Point(0., mathLine))        
            do  pf.Segments.Add( LineSegment(Point(fractionWidth, mathLine), true ))
            let pg = PathGeometry() 
            do  pg.Figures.Add(pf)
                p.Data <- pg
                p.SetValue(Grid.RowProperty,1)
            p
        let bevLine = 
            let glyph = makeGlyph textSizeFont (Element.build (Token Mo) [] [] "" (Some OperatorDictionary.divisionSlashInfix))
            makeGlyphBox glyph {y=0.;x=(getWidthFromTypeObject numerator) - MathPositioningConstants.skewedFractionHorizontalGap*0.25}

        let numeratorGrid =
            let shiftUp = 
                match display, bevelled with 
                | Inline, false -> - MathPositioningConstants.fractionNumeratorShiftUp
                | Block, false -> - (MathPositioningConstants.fractionNumeratorDisplayStyleShiftUp)
                | Block, true -> - MathPositioningConstants.fractionNumeratorShiftUp
                | Inline, true -> -(MathPositioningConstants.skewedFractionVerticalGap + MathPositioningConstants.axisHeight + (238.+ numOverHangAfter)) //
            match numerator with
            | GlyphRow gr -> 
                do gr.grid.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> 
                            gr.grid.RenderTransform <- 
                                TranslateTransform(
                                    X = numeratorHorozontalShift / 10., 
                                    Y = (mathAxisCorrectionHeight + shiftUp * (MathPositioningConstants.scriptPercentScaleDown / 100.)) / 10.
                                    )))
                gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = 
                    makeGlyphBox gl 
                        {x = numeratorHorozontalShift;
                         y = mathAxisCorrectionHeight  
                             + shiftUp * (MathPositioningConstants.scriptPercentScaleDown / 100.)}                
                do grid.Children.Add(gb) |> ignore
                grid
        let denominatorGrid =              
            let shiftDown = 
                match display, bevelled with 
                | Inline, false -> MathPositioningConstants.fractionDenominatorShiftDown
                | Block, false -> MathPositioningConstants.fractionDenominatorDisplayStyleShiftDown
                | Block, true -> MathPositioningConstants.fractionDenominatorShiftDown
                | Inline, true -> (MathPositioningConstants.skewedFractionVerticalGap + (textBaseline - MathPositioningConstants.axisHeight - MathPositioningConstants.mathLeading))
            match denominator with
            | GlyphRow gr -> 
                do gr.grid.Loaded.AddHandler(
                    RoutedEventHandler(
                        fun _ _ -> 
                            gr.grid.RenderTransform <- 
                                TranslateTransform(
                                    X = denominatorHorozontalShift / 10., 
                                    Y = (mathAxisCorrectionHeight + shiftDown * (MathPositioningConstants.scriptPercentScaleDown / 100.)) / 10.
                                    )))
                gr.grid
            | Glyph gl -> 
                let grid = Grid()
                let gb = 
                    makeGlyphBox gl 
                        {x=denominatorHorozontalShift;
                         y = mathAxisCorrectionHeight 
                             + shiftDown * (MathPositioningConstants.scriptPercentScaleDown / 100.)}
                do grid.Children.Add(gb) |> ignore
                grid
        
        let leftBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength MediumMathSpace)) * (1000./960.)
        let rightBearing = (Operator.getValueFromLength textSizeFont.emSquare (NamedLength VeryThinMathSpace)) * (1000./960.)
        let width = fractionWidth * 10.
        let height = (getHeightFromTypeObject numerator) + (getHeightFromTypeObject denominator) // + some others todo
        
        let g = Grid()
                
        do match bevelled with
           | true -> g.Children.Add(bevLine) |> ignore
           | false -> g.Children.Add(mLine) |> ignore
           List.iter (fun x -> g.Children.Add(x) |> ignore) [numeratorGrid; denominatorGrid]
        
        {grid = g;
         rowWidth = width;
         rowHeight = height;
         leftBearing = leftBearing;
         rightBearing = rightBearing;
         leftElement = GeneralLayout Mfrac;
         rightElement = GeneralLayout Mfrac;
         overHangAfter = 0.} |> GlyphRow

    // Applicators
    let applyStyleToElements (elements:Element list) (attr : MathMLAttribute list)  = 
        List.map (fun x -> Element.build x.element (List.concat[attr;x.attributes]) x.arguments x.symbol x.operator) elements

    // Typesetter    
    let typesetElement (math:Element) (el:Element) =        
        
        let typeset_Token (el:Element) = 
            match el.element = Token Mi ||
                  el.element = Token Mn ||
                  el.element = Token Mo ||
                  el.element = Token Ms ||
                  el.element = Token Mspace ||
                  el.element = Token Mtext with            
            | false -> makeGlyph (getFontFromTokenElement el) (Element.build (GeneralLayout Merror) [] [] "\ufffd" Option.None) |> Glyph
            | true -> makeGlyph (getFontFromTokenElement el) el |> Glyph
            
        let typeset_Row (el:TypeObject list) = makeRowFromTypeObjects el
      
        let typeset_Superscript ((target:TypeObject),(script:TypeObject), attributes) =             
            let manualSuperscriptShift =
                match List.tryFind (fun x -> 
                        match x with
                        | SuperScriptShift _ -> true 
                        | _ -> false) attributes with
                | Some (SuperScriptShift (Numb n)) -> n
                | _ -> 0.
            let display = 
                match List.tryFind (fun x -> 
                    match x with
                    | Display _ -> true 
                    | _ -> false) math.attributes with
                | Some (Display n) -> n
                | _ -> Inline
            let superScriptShift = 
                match List.tryFind (fun x -> 
                    match x with
                    | SuperScriptShift _ -> true 
                    | _ -> false) attributes with
                | Some (SuperScriptShift (KeyWord s)) when s.ToString() = "script" -> Inline
                | _ -> display            
            let mathAxisCorrectionHeight = 
                MathPositioningConstants.axisHeight * 
                ((MathPositioningConstants.scriptPercentScaleDown / 100.) * 
                 (1. + textSizeScaleFactor))
            let superscriptShiftUp = 
                match superScriptShift with
                | Inline -> MathPositioningConstants.superscriptShiftUpCramped + manualSuperscriptShift - mathAxisCorrectionHeight
                | Block -> MathPositioningConstants.superscriptShiftUp + manualSuperscriptShift - mathAxisCorrectionHeight
            makeSuperScriptFromTypeObjects target script superscriptShiftUp
        
        let typeset_Subscript ((target:TypeObject),(script:TypeObject), attributes) =             
            let manualSubscriptShift =
                match List.tryFind (fun x -> 
                        match x with
                        | SubScriptShift _ -> true 
                        | _ -> false) attributes with
                | Some (SubScriptShift (Numb n)) -> n
                | _ -> 0.
            let mathAxisCorrectionHeight = 
                MathPositioningConstants.axisHeight * 
                ((MathPositioningConstants.scriptPercentScaleDown / 100.) * 
                 (1. + textSizeScaleFactor))
            let subscriptShiftDown = MathPositioningConstants.subscriptShiftDown + manualSubscriptShift + mathAxisCorrectionHeight
            makeSubScriptFromTypeObjects target script subscriptShiftDown
  
        let typeset_SuperSubscript ((target:TypeObject),(superScript:TypeObject),(subScript:TypeObject), attributes) =             
            let manualSuperscriptShift =
                match List.tryFind (fun x -> 
                        match x with
                        | SuperScriptShift _ -> true 
                        | _ -> false) attributes with
                | Some (SuperScriptShift (Numb n)) -> n
                | _ -> 0.
            let display = 
                match List.tryFind (fun x -> 
                    match x with
                    | Display _ -> true 
                    | _ -> false) math.attributes with
                | Some (Display n) -> n
                | _ -> Inline
            let mathAxisCorrectionHeight = 
                MathPositioningConstants.axisHeight * 
                ((MathPositioningConstants.scriptPercentScaleDown / 100.) * 
                 (1. + textSizeScaleFactor))
            let superScriptShift = 
                match List.tryFind (fun x -> 
                    match x with
                    | SuperScriptShift _ -> true 
                    | _ -> false) attributes with
                | Some (SuperScriptShift (KeyWord s)) when s.ToString() = "script" -> Inline
                | _ -> display            
            let superscriptShiftUp = 
                match superScriptShift with
                | Inline -> MathPositioningConstants.superscriptShiftUpCramped + manualSuperscriptShift - mathAxisCorrectionHeight
                | Block -> MathPositioningConstants.superscriptShiftUp + manualSuperscriptShift - mathAxisCorrectionHeight
            
            let manualSubscriptShift =
                match List.tryFind (fun x -> 
                        match x with
                        | SubScriptShift _ -> true 
                        | _ -> false) attributes with
                | Some (SubScriptShift (Numb n)) -> n
                | _ -> 0.
            let subscriptShiftDown = MathPositioningConstants.subscriptShiftDown + manualSubscriptShift + mathAxisCorrectionHeight
            makeSuperSubScriptFromTypeObjects target superScript superscriptShiftUp subScript subscriptShiftDown

        let typeset_Overscript ((target:TypeObject),(script:TypeObject), attributes) =             
            let manualOverscriptShift =
                match List.tryFind (fun x -> 
                    match x with
                    | SuperScriptShift _ -> true 
                    | _ -> false) attributes with
                | Some (SuperScriptShift (Numb n)) -> n
                | _ -> 0.
            let display = 
                match List.tryFind (fun x -> 
                    match x with
                    | Display _ -> true 
                    | _ -> false) math.attributes with
                | Some (Display n) -> n
                | _ -> Inline
            let overScriptShift = 
                match List.tryFind (fun x -> 
                    match x with
                    | SuperScriptShift _ -> true 
                    | _ -> false) attributes with
                | Some (SuperScriptShift (KeyWord s)) when s.ToString() = "script" -> Inline
                | _ -> display            
            let mathAxisCorrectionHeight = 
                (MathPositioningConstants.stackTopShiftUp - MathPositioningConstants.stackGapMin) * //axisHeight * 
                    ((MathPositioningConstants.scriptPercentScaleDown / 100.) * (1. + textSizeScaleFactor))
            let superscriptShiftUp = 
                match overScriptShift with
                | Inline -> MathPositioningConstants.superscriptShiftUpCramped + manualOverscriptShift
                | Block -> MathPositioningConstants.stackTopDisplayStyleShiftUp + manualOverscriptShift - mathAxisCorrectionHeight

            makeOverScriptFromTypeObjects target script superscriptShiftUp display
        
        let typeset_Underscript ((target:TypeObject),(script:TypeObject), attributes) =             
            let display = 
                match List.tryFind (fun x -> 
                    match x with
                    | Display _ -> true 
                    | _ -> false) math.attributes with
                | Some (Display n) -> n
                | _ -> Inline
            let manualUnderscriptShift =
                match List.tryFind (fun x -> 
                        match x with
                        | SubScriptShift _ -> true 
                        | _ -> false) attributes with
                | Some (SubScriptShift (Numb n)) -> n
                | _ -> 0.
            let mathAxisCorrectionHeight = 
                (MathPositioningConstants.accentBaseHeight + MathPositioningConstants.stackGapMin) * 
                ((MathPositioningConstants.scriptPercentScaleDown / 100.) * 
                 (1. + textSizeScaleFactor))
            let underScriptShift = 
                match List.tryFind (fun x -> 
                    match x with
                    | SuperScriptShift _ -> true 
                    | _ -> false) attributes with
                | Some (SuperScriptShift (KeyWord s)) when s.ToString() = "script" -> Inline
                | _ -> display
            let underscriptShiftDown = 
                match underScriptShift with
                | Inline -> MathPositioningConstants.stackBottomDisplayStyleShiftDown + manualUnderscriptShift
                | Block -> MathPositioningConstants.stackBottomDisplayStyleShiftDown + manualUnderscriptShift + mathAxisCorrectionHeight
            
            makeUnderScriptFromTypeObjects target script underscriptShiftDown display
  
        let typeset_UnderOverscript ((target:TypeObject),(overScript:TypeObject),(underScript:TypeObject), attributes) =             
            let manualOverscriptShift =
                match List.tryFind (fun x -> 
                        match x with
                        | SuperScriptShift _ -> true 
                        | _ -> false) attributes with
                | Some (SuperScriptShift (Numb n)) -> n
                | _ -> 0.
            let display = 
                match List.tryFind (fun x -> 
                    match x with
                    | Display _ -> true 
                    | _ -> false) math.attributes with
                | Some (Display n) -> n
                | _ -> Inline            
            let overMathAxisCorrectionHeight = 
                (MathPositioningConstants.stackTopShiftUp - MathPositioningConstants.stackGapMin) * //axisHeight * 
                ((MathPositioningConstants.scriptPercentScaleDown / 100.) * 
                 (1. + textSizeScaleFactor))
            let overScriptShift = 
                match List.tryFind (fun x -> 
                    match x with
                    | SuperScriptShift _ -> true 
                    | _ -> false) attributes with
                | Some (SuperScriptShift (KeyWord s)) when s.ToString() = "script" -> Inline
                | _ -> display            
            let overscriptShiftUp = 
                match overScriptShift with
                | Inline -> MathPositioningConstants.superscriptShiftUpCramped + manualOverscriptShift
                | Block -> MathPositioningConstants.stackTopDisplayStyleShiftUp + manualOverscriptShift - overMathAxisCorrectionHeight
            let manualUnderscriptShift =
                match List.tryFind (fun x -> 
                        match x with
                        | SubScriptShift _ -> true 
                        | _ -> false) attributes with
                | Some (SubScriptShift (Numb n)) -> n
                | _ -> 0.
            let underMathAxisCorrectionHeight = 
                (MathPositioningConstants.accentBaseHeight + MathPositioningConstants.stackGapMin) * 
                ((MathPositioningConstants.scriptPercentScaleDown / 100.) * 
                 (1. + textSizeScaleFactor))
            let underscriptShiftDown = 
                match display with
                | Inline -> MathPositioningConstants.stackBottomDisplayStyleShiftDown + manualUnderscriptShift
                | Block -> MathPositioningConstants.stackBottomDisplayStyleShiftDown + manualUnderscriptShift + underMathAxisCorrectionHeight
            makeUnderOverScriptFromTypeObjects target  underScript underscriptShiftDown overScript overscriptShiftUp display

        let typeset_Fraction ((numerator:TypeObject),(denominator:TypeObject), attributes) = 
            let lineThickness = 
                match List.tryFind (fun x -> 
                    match x with
                    | LineThickness _ -> true 
                    | _ -> false) attributes with
                | Some (LineThickness (KeyWord k)) when k.ToString() = "medium"-> NamedLength MediumMathSpace
                | Some (LineThickness (KeyWord k)) when k.ToString() = "thick"-> NamedLength ThickMathSpace
                | Some (LineThickness (KeyWord k)) when k.ToString() = "thin"-> NamedLength ThinMathSpace
                | Some (LineThickness (Pct p)) -> Numb ((p / 100.<pct>) * MathPositioningConstants.fractionRuleThickness)
                | Some (LineThickness th) -> th
                | _ -> Numb MathPositioningConstants.fractionRuleThickness
            let numAlignment = 
                match List.tryFind (fun x -> 
                    match x with
                    | NumAlign _ -> true 
                    | _ -> false) attributes with
                | Some (NumAlign a) -> a
                | _ -> _NumAlign.Center
            let denomAlignment = 
                match List.tryFind (fun x -> 
                    match x with
                    | DenomAlign _ -> true 
                    | _ -> false) attributes with
                | Some (DenomAlign a) -> a
                | _ -> _DenomAlign.Center
            let isBevelled = 
                match List.tryFind (fun x -> 
                    match x with
                    | Bevelled _ -> true 
                    | _ -> false) attributes with
                | Some (Bevelled b) -> b
                | _ -> false
            let display = 
                match List.tryFind (fun x -> 
                    match x with
                    | Display _ -> true 
                    | _ -> false) math.attributes with
                | Some (Display n) -> n
                | _ -> Inline

            makeFractionFromTypeObjects numerator denominator lineThickness numAlignment denomAlignment isBevelled display
        
        Element.recurseElement typeset_Token 
                               typeset_Row 
                               typeset_Superscript 
                               typeset_Subscript
                               typeset_SuperSubscript
                               typeset_Overscript 
                               typeset_Underscript
                               typeset_UnderOverscript
                               typeset_Fraction el

    let typesetMath (el:Element) = ()
        
    (*Test Area*)
    type TestCanvas() as this  =  
        inherit UserControl()

        let textMath = Element.build (Math) [Display (*Inline*)Block] [] "" Option.None        

        let typesetElement el = typesetElement textMath el

        let t0 = (Element.build (Token Mo) [] [] "" (Some OperatorDictionary.cubeRootPrefix))
        let t1 = (Element.build (Token Mo) [] [] "" (Some OperatorDictionary.nArySummationPrefix))
        let t2 = (Element.build (Token Mn) [] [] "2" Option.None)
        let t3 = (Element.build (Token Mo) [] [] "" (Some OperatorDictionary.plusSignInfix))//almostEqualOrEqualToInfix))//
        let t4 = (Element.build (Token Mi) [] [] "a" Option.None)
        let t5 = (Element.build (Token Mo) [] [] "" (Some OperatorDictionary.equalsSignEqualsInfix))
        let t6 = (Element.build (Token Mi) [] [] "b" Option.None)
        let t7 = (Element.build (Token Mi) [] [] "c" Option.None)

        let s0=  (Element.build (Token Mo) [MathSize (EM 0.7<em>)] [] "" (Some OperatorDictionary.cubeRootPrefix)) 
        let s1 = (Element.build (Token Mo) [MathSize (EM 0.7<em>)] [] "" (Some OperatorDictionary.mathematicalLeftFlattenedParenthesisPrefix))
        let s2 = (Element.build (Token Mo) [MathSize (EM 0.7<em>)] [] "2" Option.None)
        let s3 = (Element.build (Token Mo) [MathSize (EM 0.7<em>); MathColor Brushes.BlueViolet] [] "" (Some OperatorDictionary.plusSignPrefix))
        let s4 = (Element.build (Token Mn) [MathSize (EM 0.7<em>)] [] "5" Option.None)
        let s5 = (Element.build (Token Mo) [MathSize (EM 0.7<em>)] [] "" (Some OperatorDictionary.mathematicalRightFlattenedParenthesisPostfix))
        let s6 = (Element.build (Token Mn) [MathSize (EM 0.7<em>)] [] "1" Option.None)
        let s7 = (Element.build (Token Mi) [MathSize (EM 0.7<em>)] [] "f" Option.None)
        let s8 = (Element.build (Token Mi) [MathSize (EM 0.7<em>)] [] "\u03C0" Option.None)
        let s9 = (Element.build (Token Mn) [MathSize (EM 0.7<em>)] [] "6" Option.None)

        let ss0=  (Element.build (Token Mo) [MathSize (EM 0.55<em>)] [] "" (Some OperatorDictionary.equalsSignEqualsInfix)) 
        let ss1 = (Element.build (Token Mo) [MathSize (EM 0.55<em>)] [] "" (Some OperatorDictionary.mathematicalLeftFlattenedParenthesisPrefix))
        let ss2 = (Element.build (Token Mi) [MathSize (EM 0.55<em>)] [] "f" Option.None)
        let ss3 = (Element.build (Token Mo) [MathSize (EM 0.55<em>); MathColor Brushes.Orchid] [] "" (Some OperatorDictionary.plusSignPrefix))
        let ss4 = (Element.build (Token Mn) [MathSize (EM 0.55<em>)] [] "1" Option.None)
        let ss5 = (Element.build (Token Mo) [MathSize (EM 0.55<em>)] [] "" (Some OperatorDictionary.mathematicalRightFlattenedParenthesisPostfix))
        let ss6 = (Element.build (Token Mn) [MathSize (EM 0.55<em>)] [] "\u221E" Option.None) 
        let ss7 = (Element.build (Token Mn) [MathSize (EM 0.55<em>)] [] "2" Option.None)

        let r0 = (Element.build (GeneralLayout Mrow) [] [t2;t3;t4] "" Option.None)
        let r1 = (Element.build (GeneralLayout Mrow) [] [s1;s2;s3;s4;s5] "" Option.None)

        let r = typesetElement (Element.build (GeneralLayout Mrow) [] [r0;r1] "" Option.None)
        let s = typesetElement (Element.build (Script Msup) [] [r0;r1] "" Option.None)

        let ms0 = (Element.build (Script Msup) [SuperScriptShift (KeyWord "script")] [s7;ss7] "" Option.None)
        let ms1 = (Element.build (Script Msup) [] [s8;ss7] "" Option.None)
        let ms2 = (Element.build (Script Msup) [] [s7;ss7] "" Option.None)

        let m = typesetElement (Element.build (Math) [] [ms0] "" Option.None)

        let f0 = (Element.build (GeneralLayout Mfrac) [ (*Bevelled true; NumAlign _NumAlign.Center*)] [s6;ms0] "" Option.None)
        let f1 = (Element.build (GeneralLayout Mfrac) [(*Bevelled true; NumAlign _NumAlign.Center*)] [ms1;s9] "" Option.None)

        let msub0 = (Element.build (Script Msub) [] [t4;s2] "" Option.None)
        let msubmsup0 = (Element.build (Script Msubsup) [] [msub0;s2] "" Option.None)
        let msubsup0 = (Element.build (Script Msubsup) [(*SubScriptShift(Numb 100.)*)] [t4;s2;s2] "" Option.None)
        let su = typesetElement (Element.build (Math) [] [msub0] "" Option.None)
        
        let msup0 = (Element.build (Script Msup) [] [t4;s2] "" Option.None)        
        let msup1 = (Element.build (Script Msup) [] [t6;s2] "" Option.None)
        let msup2 = (Element.build (Script Msup) [] [t7;s2] "" Option.None)
        let sup = typesetElement (Element.build (Math) [] [msub0] "" Option.None)
        
        let orow = (Element.build (GeneralLayout Mrow) [] [ss3;ss6] "" Option.None)
        let urow = (Element.build (GeneralLayout Mrow) [] [ss2;ss0;ss4] "" Option.None)
        let munder = (Element.build (Script Munde) [] [t1;urow] "" Option.None)
        let mover = (Element.build (Script Mover) [] [t1;orow] "" Option.None)
        let munderover = (Element.build (Script Munderover) [] [t1;orow;urow] "" Option.None)
        let uo = typesetElement (Element.build (Math) [] [munder] "" Option.None)

        let f2 = (Element.build (GeneralLayout Mfrac) [(*Bevelled true; NumAlign _NumAlign.Center*)] [ss2;ms2] "" Option.None)
        let frow = (Element.build (GeneralLayout Mrow) [] [munderover;msubsup0;t3;msup1;t5;msup2] "" Option.None)
        let f = typesetElement (Element.build (GeneralLayout Mrow) [] [munderover;f0;t5;f1] "" Option.None)

        let line3 = getGridFromTypeObject f//uo//
       
        let textBlock =                    
            let tb = TextBlock()
            tb.Text <- (getWidthFromTypeObject r) .ToString() 
            tb.FontStyle <- FontStyles.Normal
            tb.FontSize <- 60.
            tb.FontFamily <- Text.STIX2Math_FontFamily
            tb.Typography.StylisticSet1 <- true            
            tb

        let canvas = Canvas(ClipToBounds = true)
        let scale_Slider =
            let s = 
                Slider(
                    Margin = Thickness(left = 0., top = 20., right = 0., bottom = 0.),
                    Minimum = 5.,
                    Maximum = 100.,
                    TickPlacement = System.Windows.Controls.Primitives.TickPlacement.BottomRight,
                    TickFrequency = 5.,
                    IsSnapToTickEnabled = true,
                    IsEnabled = true)        
            do s.SetValue(Grid.RowProperty, 0)
            let handleValueChanged (s) = 
                line3.RenderTransform <- 
                    let tranforms = TransformGroup()
                    do  tranforms.Children.Add(TranslateTransform(X = 100., Y = 100.))
                        tranforms.Children.Add(ScaleTransform(ScaleX = 15.0/s,ScaleY = 15.0/s))            
                    tranforms
                    
            s.ValueChanged.AddHandler(RoutedPropertyChangedEventHandler(fun _ e -> handleValueChanged (e.NewValue)))
            s
        let scaleSlider_Grid =
            let g = Grid()
            do 
                g.SetValue(DockPanel.DockProperty,Dock.Top)
                g.Children.Add(scale_Slider) |> ignore
            g 
        let canvas_DockPanel =
            let d = DockPanel()
            do d.Children.Add(scaleSlider_Grid) |> ignore
            do d.Children.Add(canvas) |> ignore
            d
        let screen_Grid =
            let g = Grid()
            do g.SetValue(Grid.RowProperty, 1)        
            do g.Children.Add(canvas_DockPanel) |> ignore        
            g

        do  canvas.Children.Add(line3) |> ignore
            //canvas.Children.Add(textBlock) |> ignore
            
            this.Content <- screen_Grid