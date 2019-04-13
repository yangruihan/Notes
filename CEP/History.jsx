var docRef = app.activeDocument;
var savedState = docRef.activeHistoryState;
docRef.artLayers[0].applyMotionBlur( 20, 20 );
docRef.activeHistoryState = savedState;

// 清除历史记录
// app.purge(PurgeTarget.HISTORYCACHES);

