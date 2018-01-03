# 查找Prefab资源引用

```[c#]
using UnityEngine;
using UnityEditor;
using System.Collections.Generic;

public class ReferenceFinder : EditorWindow
{
    public static ReferenceFinder Instance;
    public Dictionary<string, BetterList<string>> dict;

    private Vector2 _scroll = Vector2.zero;

    void OnEnable() { Instance = this; }
    void OnDisable() { Instance = null; }

    void OnGUI()
    {
        if (dict == null)
        {
            return;
        }
        _scroll = GUILayout.BeginScrollView(_scroll);

        BetterList<string> list = dict["prefab"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Prefab"))
            {
                foreach (string item in list)
                {
                    GameObject go = AssetDatabase.LoadAssetAtPath(item, typeof(GameObject)) as GameObject;
                    EditorGUILayout.ObjectField("Prefab", go, typeof(GameObject), false);

                }
            }
            list = null;
        }

        list = dict["fbx"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("FBX"))
            {
                foreach (string item in list)
                {
                    GameObject go = AssetDatabase.LoadAssetAtPath(item, typeof(GameObject)) as GameObject;
                    EditorGUILayout.ObjectField("FBX", go, typeof(GameObject), false);

                }
            }
            list = null;
        }

        list = dict["cs"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Script"))
            {
                foreach (string item in list)
                {
                    MonoScript go = AssetDatabase.LoadAssetAtPath(item, typeof(MonoScript)) as MonoScript;
                    EditorGUILayout.ObjectField("Script", go, typeof(MonoScript), false);

                }
            }
            list = null;
        }

        list = dict["texture"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Texture"))
            {
                foreach (string item in list)
                {
                    Texture2D go = AssetDatabase.LoadAssetAtPath(item, typeof(Texture2D)) as Texture2D;
                    EditorGUILayout.BeginHorizontal();
                    EditorGUILayout.TextField(item);
                    EditorGUILayout.ObjectField("", go, typeof(Texture2D), false);
                    EditorGUILayout.EndHorizontal();
                }
            }
            list = null;
        }

        list = dict["mat"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Material"))
            {
                foreach (string item in list)
                {
                    Material go = AssetDatabase.LoadAssetAtPath(item, typeof(Material)) as Material;
                    EditorGUILayout.ObjectField("Material", go, typeof(Material), false);

                }
            }
            list = null;
        }

        list = dict["shader"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Shader"))
            {
                foreach (string item in list)
                {
                    Shader go = AssetDatabase.LoadAssetAtPath(item, typeof(Shader)) as Shader;
                    EditorGUILayout.ObjectField("Shader", go, typeof(Shader), false);
                }
            }
            list = null;
        }

        list = dict["font"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Font"))
            {
                foreach (string item in list)
                {
                    Font go = AssetDatabase.LoadAssetAtPath(item, typeof(Font)) as Font;
                    EditorGUILayout.ObjectField("Font", go, typeof(Font), false);
                }
            }
            list = null;
        }

        list = dict["anim"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Animation"))
            {
                foreach (string item in list)
                {
                    AnimationClip go = AssetDatabase.LoadAssetAtPath(item, typeof(AnimationClip)) as AnimationClip;
                    EditorGUILayout.ObjectField("Animation:", go, typeof(AnimationClip), false);
                }
            }
            list = null;
        }

        list = dict["animTor"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Animator"))
            {
                foreach (string item in list)
                {
                    //Animator go = AssetDatabase.LoadAssetAtPath(item, typeof(Animator)) as Animator;
                    //EditorGUILayout.ObjectField("Animator:", go, typeof(Animator), true);
                    EditorGUILayout.LabelField(item);
                }
            }
            list = null;
        }

        list = dict["level"];
        if (list != null && list.size > 0)
        {
            if (DrawHeader("Level"))
            {
                foreach (string item in list)
                {
                    //SceneView go = AssetDatabase.LoadAssetAtPath(item, typeof(SceneView)) as SceneView;
                    //EditorGUILayout.ObjectField("Animation:" , go, typeof(SceneView), true);
                    EditorGUILayout.LabelField(item);
                }
            }
            list = null;
        }

        GUILayout.EndScrollView();
    }

    /// <summary>
    /// 根据脚本查找引用的对象
    /// </summary>
    [MenuItem("Assets/Reference/Find Script Reference", false, 0)]
    static public void FindScriptReference()
    {
        ShowProgress(0, 0, 0);
        string curPathName = AssetDatabase.GetAssetPath(Selection.activeObject.GetInstanceID());

        Dictionary<string, BetterList<string>> dic = new Dictionary<string, BetterList<string>>();
        BetterList<string> prefabList = new BetterList<string>();
        BetterList<string> fbxList = new BetterList<string>();
        BetterList<string> scriptList = new BetterList<string>();
        BetterList<string> textureList = new BetterList<string>();
        BetterList<string> matList = new BetterList<string>();
        BetterList<string> shaderList = new BetterList<string>();
        BetterList<string> fontList = new BetterList<string>();
        BetterList<string> levelList = new BetterList<string>();

        string[] allGuids = AssetDatabase.FindAssets("t:Prefab t:Scene", new string[] { "Assets" });
        int i = 0;
        foreach (string guid in allGuids)
        {
            string assetPath = AssetDatabase.GUIDToAssetPath(guid);
            string[] names = AssetDatabase.GetDependencies(new string[] { assetPath });
            foreach (string name in names)
            {
                if (name.Equals(curPathName))
                {
                    if (assetPath.EndsWith(".prefab"))
                    {
                        prefabList.Add(assetPath);
                        break;
                    }
                    else if (assetPath.ToLower().EndsWith(".fbx"))
                    {
                        fbxList.Add(assetPath);
                        break;
                    }
                    else if (assetPath.ToLower().EndsWith(".unity"))
                    {
                        levelList.Add(assetPath);
                        break;
                    }
                    else if (assetPath.EndsWith(".cs"))
                    {
                        scriptList.Add(assetPath);
                        break;
                    }
                    else if (assetPath.EndsWith(".png"))
                    {
                        textureList.Add(assetPath);
                        break;
                    }
                    else if (assetPath.EndsWith(".mat"))
                    {
                        matList.Add(assetPath);
                        break;
                    }
                    else if (assetPath.EndsWith(".shader"))
                    {
                        shaderList.Add(assetPath);
                        break;
                    }
                    else if (assetPath.EndsWith(".ttf"))
                    {
                        fontList.Add(assetPath);
                        break;
                    }
                }
            }
            ShowProgress((float)i / (float)allGuids.Length, allGuids.Length, i);
            i++;
        }

        dic.Add("prefab", prefabList);
        dic.Add("fbx", fbxList);
        dic.Add("cs", scriptList);
        dic.Add("texture", textureList);
        dic.Add("mat", matList);
        dic.Add("shader", shaderList);
        dic.Add("font", fontList);
        dic.Add("level", levelList);
        dic.Add("anim", null);
        dic.Add("animTor", null);
        EditorUtility.ClearProgressBar();
        EditorWindow.GetWindow<ReferenceFinder>(false, "Object Reference", true).Show();

        if (ReferenceFinder.Instance.dict != null) ReferenceFinder.Instance.dict.Clear();
        ReferenceFinder.Instance.dict = dic;
    }

    public static void ShowProgress(float val, int total, int cur)
    {
        EditorUtility.DisplayProgressBar("Searching", string.Format("Finding ({0}/{1}), please wait...", cur, total), val);
    }

    /// <summary>
    /// 查找对象引用的类型
    /// </summary>
    [MenuItem("Assets/Reference/Find Object Dependencies", false, 0)]
    public static void FindObjectDependencies()
    {

        ShowProgress(0, 0, 0);
        Dictionary<string, BetterList<string>> dic = new Dictionary<string, BetterList<string>>();
        BetterList<string> prefabList = new BetterList<string>();
        BetterList<string> fbxList = new BetterList<string>();
        BetterList<string> scriptList = new BetterList<string>();
        BetterList<string> textureList = new BetterList<string>();
        BetterList<string> matList = new BetterList<string>();
        BetterList<string> shaderList = new BetterList<string>();
        BetterList<string> fontList = new BetterList<string>();
        BetterList<string> animList = new BetterList<string>();
        BetterList<string> animTorList = new BetterList<string>();
        string curPathName = AssetDatabase.GetAssetPath(Selection.activeObject.GetInstanceID());
        string[] names = AssetDatabase.GetDependencies(new string[] { curPathName });  //依赖的东东
        int i = 0;
        foreach (string name in names)
        {
            if (name.EndsWith(".prefab"))
            {
                prefabList.Add(name);
            }
            else if (name.ToLower().EndsWith(".fbx"))
            {
                fbxList.Add(name);
            }
            else if (name.EndsWith(".cs"))
            {
                scriptList.Add(name);
            }
            else if (name.EndsWith(".png"))
            {
                textureList.Add(name);
            }
            else if (name.EndsWith(".mat"))
            {
                matList.Add(name);
            }
            else if (name.EndsWith(".shader"))
            {
                shaderList.Add(name);
            }
            else if (name.EndsWith(".ttf"))
            {
                fontList.Add(name);
            }
            else if (name.EndsWith(".anim"))
            {
                animList.Add(name);
            }
            else if (name.EndsWith(".controller"))
            {
                animTorList.Add(name);
            }
            Debug.Log("Dependence:" + name);
            ShowProgress((float)i / (float)names.Length, names.Length, i);
            i++;
        }

        dic.Add("prefab", prefabList);
        dic.Add("fbx", fbxList);
        dic.Add("cs", scriptList);
        dic.Add("texture", textureList);
        dic.Add("mat", matList);
        dic.Add("shader", shaderList);
        dic.Add("font", fontList);
        dic.Add("level", null);
        dic.Add("animTor", animTorList);
        dic.Add("anim", animList);
        EditorWindow.GetWindow<ReferenceFinder>(false, "Object Dependencies", true).Show();
        if (ReferenceFinder.Instance.dict != null) ReferenceFinder.Instance.dict.Clear();
        ReferenceFinder.Instance.dict = dic;
        EditorUtility.ClearProgressBar();
    }

    static public bool DrawHeader(string text)
    {
        bool state = EditorPrefs.GetBool(text, true);
        GUILayout.BeginHorizontal();
        GUI.changed = false;

        text = "\u25BA" + (char)0x200a + text;

        GUILayout.BeginHorizontal();
        GUI.contentColor = EditorGUIUtility.isProSkin ? new Color(1f, 1f, 1f, 0.7f) : new Color(0f, 0f, 0f, 0.7f);
        if (!GUILayout.Toggle(true, text, "PreToolbar2", GUILayout.MinWidth(20f)))
            state = !state;
        GUI.contentColor = Color.white;
        GUILayout.EndHorizontal();

        if (GUI.changed) EditorPrefs.SetBool(text, state);


        GUILayout.EndHorizontal();
        GUI.backgroundColor = Color.white;
        return state;
    }
}
```
