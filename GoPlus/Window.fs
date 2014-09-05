module Window

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

type Window () as this =
    inherit Form ()

    do
        this.Text <- "GoPlus"
        this.ClientSize <- new Size(640, 480)
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.White
    //The switch between length1 being y and length2 being x was just a product of how they were iterated over. Make it consistent and make it print consistently. Change the printing to suit a logical foundation, not the other way around
    // Window will handle timer and whose turn it is, it will translate ui actions into function calls on Game.
    // It decides when things happen, Game implements them
    override this.OnMouseMove args =
        printfn "%i, %i" args.X args.Y