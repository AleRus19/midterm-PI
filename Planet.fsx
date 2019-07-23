open System.Windows.Forms
open System.Drawing


type WVMatrix () =
  let wv = new Drawing2D.Matrix()
  let vw = new Drawing2D.Matrix()
  
  member this.TranslateW (tx, ty) =
    wv.Translate(tx, ty)
    vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleW (sx, sy) =
    wv.Scale(sx, sy)
    vw.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

  member this.RotateW (a) =
    wv.Rotate(a)
    vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.RotateV (a) =
    vw.Rotate(a)
    wv.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.TranslateV (tx, ty) =
    vw.Translate(tx, ty)
    wv.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleV (sx, sy) =
    vw.Scale(sx, sy)
    wv.Scale(1.f /sx, 1.f/ sy, Drawing2D.MatrixOrder.Append)

 
  member this.TransformPointV (p:PointF) =
    let a = [| p |]
    vw.TransformPoints(a)
    a.[0]

  member this.TransformPointW (p:PointF) =
    let a = [| p |]
    wv.TransformPoints(a)
    a.[0]

  member this.VW with get() = vw
  member this.WV with get() = wv


 type LWCControl() =
 
  let  wv = WVMatrix() //dico la sua matrice di trasformazione, inizialmente matrice identica
  let mutable sz = SizeF(120.f, 120.f) //dimensione a caso
  let mutable pos = PointF()
  let mutable parent : LWCContainer option = None
  let mutable c= Brushes.Blue
  let mutable light= Brushes.Yellow
  let mutable dragged = false
  let mutable img = null
  let mutable typeLwc = -1
  let mutable  op = -1;
  
  member this.WV = wv
  
  member this.Image
    with get() = img
    and set(v:Bitmap) = img <- v
  
  
  member this.Parent
    with get() = parent
    and set(v) = parent <- v

  member this.Type
    with get() = typeLwc
    and set(v) = typeLwc <- v

    
  abstract OnPaint : PaintEventArgs -> unit //segnatura
  default this.OnPaint (e) = ()//implementazione di default
  
  abstract OnMouseDown : MouseEventArgs -> unit
  default this.OnMouseDown (e) = ()
 
  abstract OnMouseUp : MouseEventArgs -> unit
  default this.OnMouseUp (e) = ()

  abstract OnMouseMove : MouseEventArgs -> unit
  default this.OnMouseMove (e) = ()


  member this.Invalidate() =
    match parent with
    | Some p -> p.Invalidate()
    | None -> ()

  //il mouse in che coordinate mi arriva : mi arriva in coordinate vista
  member this.HitTest(p:Point) =
    let pt = wv.TransformPointV(PointF(single p.X, single p.Y))
    let boundingbox = RectangleF(0.f, 0.f, sz.Width, sz.Height)
    boundingbox.Contains(pt)

  member this.ClientSize
    with get() = sz
    and set(v) = sz <- v 
                 this.Invalidate()
  
  
  //vorrei che la posizione fosse scritta nella matrice
  //illusione di un sistema di coordinate tutto suo 
  member this.Position
    with get() = pos
    and set(v) =
      wv.TranslateV(pos.X, pos.Y)//vorrei traslare la vista
      pos <- v
      wv.TranslateV(-pos.X, -pos.Y)
      this.Invalidate()
  
   member this.Color
    with get() = c
    and set(v) =
      c <- v
   
   member this.Operation
    with get() = op
    and set(v) = op <- v
  
   member this.Light
    with get() = light
    and set(v) =
      light <- v
   member this.Drag
    with get() = dragged
    and set(v) =
      dragged <- v
  
   
  //restituisce un punto ovvero il vertice
  member this.PositionInt with get() = Point(int pos.X, int pos.Y) //per creare un rettangolo vero intero
  //restituisce la dimensione del rettangolo
  member this.ClientSizeInt with get() = Size(int sz.Width, int sz.Height)

  member this.Left = pos.X
  member this.Top = pos.Y
  member this.Width = sz.Width //short end
  member this.Height = sz.Height 

//let mutable imaging = false;


//and = questi tipi sono definiti insieme e si devono vedere 
//abbiamo bisogno del nostro contenitore di controlli
and LWCContainer() as this =
  inherit UserControl()
  let mutable newbox = None
  let mutable drag = None
 // let remove = new Button(Text = "Remove", Location=new Point(80,60))
  let image = new Button(Text="Image",Location=new Point(160,60))
  let mutable removing = false;
  let mutable imaging = false ;
  let mutable create = false;
  let mutable speed = 0;
  let timer = new Timer (Interval=20)
  let timer_end = new Timer (Interval=20)
  let mutable planet = LWCControl()
  let mutable bl=false
  let mkrect (sx, sy) (ex, ey) =
        Rectangle(min sx ex, min sy ey, abs(sx - ex), abs(sy - ey))
  let controls = System.Collections.ObjectModel.ObservableCollection<LWCControl>()
  
  do this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
     controls.CollectionChanged.Add(fun e ->
       for i in e.NewItems do
        (i :?> LWCControl).Parent <- Some(this)
       )
     controls.Add(Navicella(Type=1,Color=Brushes.Green,Position=PointF(100.f,100.f),ClientSize=SizeF(single(abs(10 - 50)),single(abs(10 - 50)))))
     controls.Add(LWButton(Text="Left",Type=3,Operation=1,Color=Brushes.LightGreen,Position=PointF(10.f,10.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="Rigth",Type=3,Operation=2,Color=Brushes.LightGreen,Position=PointF(80.f,10.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="Up",Type=3,Operation=3,Color=Brushes.LightGreen,Position=PointF(150.f,10.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="Down",Type=3,Operation=4,Color=Brushes.LightGreen,Position=PointF(220.f,10.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="RotateSx",Type=3,Operation=5,Color=Brushes.LightGreen,Position=PointF(10.f,50.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="RotateDx",Type=3,Operation=6,Color=Brushes.LightGreen,Position=PointF(80.f,50.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="ZoomIn",Type=3,Operation=7,Color=Brushes.LightGreen,Position=PointF(150.f,50.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="ZoomOut",Type=3,Operation=8,Color=Brushes.LightGreen,Position=PointF(220.f,50.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="Image",Type=3,Operation=0,Color=Brushes.LightGreen,Position=PointF(290.f,50.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="Planet",Type=3,Operation=9,Color=Brushes.LightGreen,Position=PointF(290.f,10.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="+",Type=3,Operation=10,Color=Brushes.LightGreen,Position=PointF(400.f,10.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="-",Type=3,Operation=11,Color=Brushes.LightGreen,Position=PointF(400.f,50.f),ClientSize=SizeF(60.f,30.f)))
     controls.Add(LWButton(Text="End",Type=3,Operation=12,Color=Brushes.LightGreen,Position=PointF(400.f,90.f),ClientSize=SizeF(60.f,30.f)))
   
    
   
     (*timer.Tick.Add(fun _ -> 
            controls |>Seq.iter (fun c ->
              if c.Type=1 then 
                c.WV.TranslateW(0.f, -2.f) 
                this.Invalidate()
           )
     )
     *)
     timer_end.Tick.Add(fun _ -> 
            controls |>Seq.iter (fun c ->
              if c.Type=1 then 
                if speed > 20   then 
                  c.WV.TranslateW(0.f, -2.f) 
                  speed <- speed- 1
                if speed > 10 then 
                  c.WV.TranslateW(0.f, -1.f) 
                  speed <- speed- 1
  
                this.Invalidate()
            )
            
     )
  


  member this.LWControls with get() = controls
  member this.Removee 
     with get() = removing
     and set(v) = removing <- v
  
  
  member this.Left () = 
                controls |> Seq.iter (fun c->
                                            if c.Type <> 3 then 
                                                c.WV.TranslateV(10.f,0.f) 
                                                this.Invalidate()
            )
  member this.Right () =
                controls |> Seq.iter ( fun c-> 
                                            if c.Type <> 3 then 
                                              c.WV.TranslateV(-10.f,0.f)
                                              this.Invalidate()
                                  )
     
  
  member this.Up () =
                controls |> Seq.iter ( fun c-> 
                                            if c.Type <> 3 then 
                                              c.WV.TranslateV(0.f,10.f)
                                              this.Invalidate()
                                  )   


  member this.Down () =
                controls |> Seq.iter ( fun c-> 
                                            if c.Type <> 3 then 
                                              c.WV.TranslateV(0.f,-10.f)
                                              this.Invalidate()
                                  )   
  
  member this.RotateSx () =
                controls |> Seq.iter ( fun c-> 
                                            if c.Type <> 3 then 
                                              let cx,cy =single  this.ClientSize.Width/2.f, single this.ClientSize.Height /2.f
                                              c.WV.TranslateV(cx,cy)
                                              c.WV.RotateV(-10.f)
                                              c.WV.TranslateV(-cx,-cy)
                                              this.Invalidate()
                                  )   
  
  member this.RotateDx () =
                controls |> Seq.iter ( fun c-> 
                                            if c.Type <> 3 then 
                                              let cx,cy =single  this.ClientSize.Width/2.f, single this.ClientSize.Height /2.f
                                              c.WV.TranslateV(cx,cy)
                                              c.WV.RotateV(10.f)
                                              this.Invalidate()
                                              c.WV.TranslateV(-cx,-cy)
                                              
                                  )   
  
  member this.ZoomIn () =
                controls |> Seq.iter ( fun c-> 
                                            if c.Type <> 3 then 
                                              let cx,cy =single  this.ClientSize.Width/2.f, single this.ClientSize.Height /2.f
                                              c.WV.TranslateV(cx,cy)
                                              c.WV.ScaleV(1.1f,1.1f)
                                              c.WV.TranslateV(-cx,-cy)
                                              this.Invalidate()
                                  )   
  member this.ZoomOut () =
                controls |> Seq.iter ( fun c-> 
                                            if c.Type <> 3 then 
                                              let cx,cy =single  this.ClientSize.Width/2.f, single this.ClientSize.Height /2.f
                                              c.WV.TranslateV(cx,cy)
                                              c.WV.ScaleV(1.f/1.1f,1.f/1.1f)
                                              c.WV.TranslateV(-cx,-cy)
                                              this.Invalidate()
                                  )   
  
  member this.Image () = imaging <-true
  member this.Create () = create <-true
  member this.More() =
                                              let cx,cy =planet.Width/2.f, single planet.Height /2.f
                                              planet.WV.TranslateW(cx,cy)
                                              planet.WV.ScaleW(1.1f,1.1f)
                                              planet.WV.TranslateW(-cx,-cy)
                                              this.Invalidate()
  member this.Less() =
                                              let cx,cy =planet.Width/2.f, single planet.Height /2.f
                                              planet.WV.TranslateW(cx,cy)
                                              planet.WV.ScaleW(1.f/1.1f,1.f/1.1f)
                                              planet.WV.TranslateW(-cx,-cy)
                                              this.Invalidate()
  member this.End() =  bl <- false      
  
  override this.OnKeyDown e =
     if e.KeyCode = Keys.Q && e.KeyCode = Keys.W  then 
                timer_end.Stop()
                let mutable target = LWCControl() 
                controls |> Seq.iter(fun c->
                                             if c.Type = 1 then 
                                                target <- c
                                                c.WV.TranslateW(0.f, -10.f) 
                                                let cx, cy = c.Width/2.f , c.Height / 2.f  
                                                c.WV.TranslateW(cx, cy)
                                                c.WV.RotateW(-10.f)
                                                c.WV.TranslateW(-cx, -cy)
                                                speed <- speed+ 3
                                                
                                     )
                controls.Move(controls.IndexOf(target),controls.Count-1)
                this.Invalidate()
     if e.KeyCode = Keys.E && e.KeyCode = Keys.W  then 
                timer_end.Stop()
                let mutable target = LWCControl() 
                controls |> Seq.iter(fun c->
                                             if c.Type = 1 then 
                                                target <- c
                                                c.WV.TranslateW(0.f, -10.f) 
                                                let cx, cy = c.Width/2.f , c.Height / 2.f  
                                                c.WV.TranslateW(cx, cy)
                                                c.WV.RotateW(10.f)
                                                c.WV.TranslateW(-cx, -cy)
                                                speed <- speed+ 3
                                                
                                     )
                controls.Move(controls.IndexOf(target),controls.Count-1)
                this.Invalidate()
    
    
      else if e.KeyCode = Keys.W then  
                timer_end.Stop()
                let mutable target = LWCControl() 
                controls |> Seq.iter(fun c->
                                             if c.Type = 1 then 
                                                target <- c
                                                c.WV.TranslateW(0.f, -10.f) 
                                                speed <- speed+ 3
                                                
                                     )
                controls.Move(controls.IndexOf(target),controls.Count-1)
                this.Invalidate()
                                                // wv *= T(0,10)
      else  if e.KeyCode= Keys.E then    
                   let mutable target = LWCControl() 
                   controls |> Seq.iter(fun c->
                                             if c.Type = 1 then 
                                               target <- c
                                               let cx, cy = c.Width/2.f , c.Height / 2.f  
                                               c.WV.TranslateW(cx, cy)
                                               c.WV.RotateW(10.f)
                                               c.WV.TranslateW(-cx, -cy)
                                        )
                   controls.Move(controls.IndexOf(target),controls.Count-1)
                   this.Invalidate()
               
      else if e.KeyCode= Keys.Q  then 
                   let mutable target = LWCControl() 
                   controls |> Seq.iter(fun c->
                                             if c.Type = 1 then 
                                               target  <-c
                                               let cx, cy = c.Width/2.f , c.Height / 2.f  
                                               c.WV.TranslateW(cx, cy)
                                               c.WV.RotateW(-10.f)
                                               c.WV.TranslateW(-cx, -cy)
                                        )
                   controls.Move(controls.IndexOf(target),controls.Count-1)
                   this.Invalidate()
              
    


   override this.OnKeyUp e =
    match e.KeyCode with
     |Keys.W -> //timer.Stop()
                timer_end.Start()
     | _ ->()
  

  override this.OnMouseDown (e) =  
    let oc = //ricerca lineare incerta , prova a tornare indietro dall'ultimo al primo
             //siccome disegno da sinistra a destra, devo cercare da destra a sinistra
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      
    //trasformiamo le coordinate vista in coordinate mondo 
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseDown(evt)
      if c.Type<>3 then 
        planet <- c
        bl<-true
    
      this.Invalidate()
      let dx = e.X - int c.Left
      let dy = e.Y - int c.Top
      if c.Type <> 3 then 
        if imaging=true then 
           let dlg = new OpenFileDialog()
           dlg.Filter <- "|*.BMP;*.JPG;*.GIF;*.PNG"
           if dlg.ShowDialog() = DialogResult.OK then
             let imagename = dlg.FileName
             let myPicture : Bitmap = new Bitmap(imagename)
             c.Image <- myPicture
             imaging <- false
      if c.Type <> 3 then 
        drag <- Some(c,dx,dy)
    | None ->  if create = true then newbox <-Some(e.X,e.Y)
               
                        
  override this.OnMouseUp (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseUp(evt)
      drag <-None
      this.Invalidate()
     
    | None -> () 
                              
    match newbox with 
    |Some(sx,sy) ->           let dlg = new OpenFileDialog()
                              dlg.Filter <- "|*.BMP;*.JPG;*.GIF;*.PNG"
                              if dlg.ShowDialog() = DialogResult.OK then
                                let imagename = dlg.FileName
                                printfn "%A" imagename
                                let myPicture : Bitmap = new Bitmap(imagename)
                                controls.Add(Pianeti(Type=0,Image=myPicture,Position=PointF(single sx,single sy),ClientSize=SizeF(100.f,100.f)))
                                newbox <-None
                                create <- false
                                this.Invalidate()
    |None -> ()

   
  override this.OnMouseMove (e) =
    let oc =
      controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseMove(evt)    
      this.Invalidate()
    | None -> () 
    match drag with 
      |Some (c,dx,dy) -> 
                         c.Position <- PointF(single (e.X-dx),single (e.Y-dy))  
                         this.Invalidate()
      |_ -> ()
  
   
  override this.OnPaint(e) =
    controls |> Seq.iter(fun c ->
      let bkg = e.Graphics.Save()//si salva lo stato e poi si ripristina per renderlo il più possibile pulito
      let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.ClientSizeInt))
      //evt.Graphics.SetClip(new RectangleF(c.Position, c.ClientSize))//un vertice e una dimensione, il clipping non supporta la rotazione non abbiamo assunto che è ruotato
      
      evt.Graphics.Transform <- c.WV.WV //cambiamo la matrice di trasformazione
      if c.Operation <> 10 && c.Operation <> 11 && c.Operation <> 12 then  
        c.OnPaint(evt)//passiamo all'onpaint un nuovo evento, prima passavamo quello del padre
      else if bl=true then 
        c.OnPaint(evt)//passiamo all'onpaint un nuovo evento, prima passavamo quello del padre
      //così è ritagliato nel suo spazio di coordinate
      e.Graphics.Restore(bkg)
    )
    

// Utente Libreria
and Pianeti() =
  inherit LWCControl()
  let mutable drag= None

  override this.OnPaint(e) =
    let g = e.Graphics
    if not (isNull this.Image) then 
        g.DrawImage(this.Image, RectangleF(0.f,0.f,this.Width,this.Height))
    else 
        g.FillRectangle(this.Color, 0.f, 0.f, this.Width, this.Height)
       
    
  override this.OnMouseDown(e) =
     printfn "%A" e.Location
  

  override this.OnMouseMove e =
     printfn " " 
  
  override this.OnMouseUp(e) =
   printfn " "


and LWButton() =
  inherit LWCControl()
  let mutable drag= None
  let mutable txt = " "
  let mutable cont  = ResizeArray<LWCControl>()




  member this.Text
    with get() = txt
    and set(v) = txt <- v
  
  (*member this.Container
    with get() = cont
    and set(v:ResizeArray<LWCControl>) = cont <- v*)

  override this.OnPaint(e) =
    let g = e.Graphics
    if not (isNull this.Image) then 
        g.DrawImage(this.Image, RectangleF(0.f,0.f,this.Width,this.Height))
    else 
        g.FillRectangle(this.Color, 0.f, 0.f, this.Width, this.Height)
        g.DrawString(this.Text,SystemFonts.DefaultFont, System.Drawing.Brushes.Blue, new RectangleF(0.f,0.f,this.Width |> single,this.Height |>single));

  override this.OnMouseDown(e) =
    printfn "button"
    match this.Parent with 
     |Some p ->     
                     if this.Operation = 0  then 
                        p.Image()
                     if this.Operation = 1  then 
                        p.Left()
                     if this.Operation = 2  then 
                        p.Right()
                     if this.Operation = 3  then 
                        p.Up()
                     if this.Operation = 4  then 
                        p.Down()
                     if this.Operation = 5  then 
                        p.RotateSx()
                     if this.Operation = 6  then 
                        p.RotateDx()
                     if this.Operation = 7  then 
                        p.ZoomIn()
                     if this.Operation = 8  then 
                        p.ZoomOut()
                     if this.Operation = 9  then 
                        p.Create()
                     if this.Operation = 10  then 
                        p.More()
                     if this.Operation = 11  then 
                        p.Less()
                     if this.Operation = 12  then 
                        p.End()
                                 
                         
                     
     |_ -> ()
   
    
  override this.OnMouseMove e =
     printfn " " 
  
  override this.OnMouseUp(e) =
   printfn " "
    

    
    
and Navicella() =
  inherit LWCControl()
  
  override this.OnPaint(e) =
    let g = e.Graphics
    if not (isNull this.Image) then 
        g.DrawImage(this.Image, RectangleF(0.f,0.f,this.Width,this.Height))
    else
        g.FillRectangle(this.Color, 0.f, 0.f, this.Width, this.Height)
        g.DrawString("Image ship",SystemFonts.DefaultFont, System.Drawing.Brushes.White, new RectangleF(0.f,0.f,this.Width |> single,this.Height |>single));
 
    
  override this.OnMouseDown(e) =
     printfn "%A" e.Location
  

  override this.OnMouseMove e =
     printfn " " 
  

let lwcc = new LWCContainer(Dock=DockStyle.Fill) //deve occupare tutto lo spazio
let f = new Form(Text="Prova",MinimumSize=Size(1000,1000))
f.Controls.Add(lwcc)
f.Show()
