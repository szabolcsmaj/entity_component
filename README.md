# TODO:

 - After parsing possible object values, update model to use these instead of the plain old string.
 - Implement collapse/expand for child nodes.
 - Store if the expanding of the object requires a server call. This might be shown to the UI too so the end users are aware as well (e.g.: another color for expand)
 - Parse java.util.HashMap ({Key Name=value,Another Key=Another Value})
 - Parse java.util.ArrayList ([Object1, Object2])
 - Refactor the HTML rendition parts out into separate modul which will be responsible for styling the output
 - Investigate elm-css
 - Investigate whether the numbering of objects should be done on the server side or here
 - Devise a method to deal with nodes that are extremely large. This should be probably done on the server side and based on a more complex evaluation. It would make sense to check the size of the whole root object. If it's bigger than ~1-2 megs, we know we have to reduce it's size. We can then either send every big object (bigger than say 0.5 MB) collapsed by default. Identification of objects becomes important here.

