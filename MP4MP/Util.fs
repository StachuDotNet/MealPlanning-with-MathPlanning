module MP4MP.Util

let printList heading list =
    printfn "%s: %A" heading (String.concat ", " list)