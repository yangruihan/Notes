var docRef = app.documents.add(500, 500);
docRef.artLayers.add();

var shapeRef = [
    [0, 0],
    [0, 300],
    [300, 300],
    [300, 0]
]

var strokeColor = new SolidColor();
strokeColor.cmyk.cyan = 20;
strokeColor.cmyk.magenta = 50;
strokeColor.cmyk.yellow = 30;
strokeColor.cmyk.black = 20;

docRef.selection.select(shapeRef);
docRef.selection.stroke(strokeColor, 2, StrokeLocation.OUTSIDE,
                        ColorBlendMode.VIVIDLIGHT, 75, false);

var fillColor = new SolidColor();
fillColor.rgb.red = 255;
fillColor.rgb.green = 200;
fillColor.rgb.blue = 0;
docRef.selection.fill(fillColor, ColorBlendMode.VIVIDLIGHT, 25, false);

