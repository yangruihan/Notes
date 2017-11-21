# Unity 背景动态模糊(blur)和截屏方法（转）

原文点[这里](http://blog.csdn.net/tom_221x/article/details/50764025)

Unity 提供了一个函数`OnRenderImage(RenderTexture source, RenderTexture destination)`，只要将这个组件挂载在 Camera 上面。就能够自动调用，获得当前屏幕的 RenderTexture，和需要写入的目标 RenderTexture，也就是需要显示的纹理。可见，source 就是当前屏幕的纹理数据，可以保存这个数据得到当前屏幕的纹理数据，就相当于截图了，我们也可以通过剪裁采样这个数据，得到一个放大或缩小的截图数据使用。

根据这个接口，Unity官方给予了一个blur动态模糊的效果。代码如下：

```[c#]
using UnityEngine;
using System.Collections;
using UnityEngine.UI;
using DG;
using DG.Tweening;

[ExecuteInEditMode]
[AddComponentMenu("Image Effects/Blur/Blur")]
public class BlurEffect : MonoBehaviour
{

    /// Blur iterations - larger number means more blur.
    public int iterations = 3;

    /// Blur spread for each iteration. Lower values
    /// give better looking blur, but require more iterations to
    /// get large blurs. Value is usually between 0.5 and 1.0.
    public float blurSpread = 0.6f;


    // --------------------------------------------------------
    // The blur iteration shader.
    // Basically it just takes 4 texture samples and averages them.
    // By applying it repeatedly and spreading out sample locations
    // we get a Gaussian blur approximation.

    public Shader blurShader = null;

    //private static string blurMatString =

    static Material m_Material = null;
    protected Material material {
        get {
            if (m_Material == null) {
                m_Material = new Material(blurShader);
                m_Material.hideFlags = HideFlags.DontSave;
            }
            return m_Material;
        }
    }

    protected void OnDisable() {
        if( m_Material ) {
            DestroyImmediate( m_Material );
        }

        rawImage = null;
    }

    // --------------------------------------------------------

    protected void Start()
    {
        // Disable if we don't support image effects
        if (!SystemInfo.supportsImageEffects) {
            enabled = false;
            return;
        }
        // Disable if the shader can't run on the users graphics card
        if (!blurShader || !material.shader.isSupported) {
            enabled = false;
            return;
        }
    }

    // Performs one blur iteration.
    public void FourTapCone (RenderTexture source, RenderTexture dest, int iteration)
    {
        float off = 0.5f + iteration*blurSpread;
        Graphics.BlitMultiTap (source, dest, material,
            new Vector2(-off, -off),
            new Vector2(-off,  off),
            new Vector2( off,  off),
            new Vector2( off, -off)
        );
    }

    // Downsamples the texture to a quarter resolution.
    private void DownSample4x (RenderTexture source, RenderTexture dest)
    {
        float off = 1.0f;
        Graphics.BlitMultiTap (source, dest, material,
            new Vector2(-off, -off),
            new Vector2(-off,  off),
            new Vector2( off,  off),
            new Vector2( off, -off)
        );
    }

    // Called by the camera to apply the image effect
    void OnRenderImage (RenderTexture source, RenderTexture destination)
    {
        int rtW = source.width  / 4;
        int rtH = source.height / 4;
        RenderTexture buffer = RenderTexture.GetTemporary(rtW, rtH, 0);

        // Copy source to the 4x4 smaller texture.
        DownSample4x (source, buffer);

        // Blur the small texture
        for(int i = 0; i < iterations; i++)
        {
            RenderTexture buffer2 = RenderTexture.GetTemporary(rtW, rtH, 0);
            FourTapCone (buffer, buffer2, i);
            RenderTexture.ReleaseTemporary(buffer);
            buffer = buffer2;
        }

        Graphics.Blit(buffer, destination);
        RenderTexture.ReleaseTemporary(buffer);
    }
}
```

代码需要挂载在 Camera 下面，通过算法计算 source 的纹理数据，然后调用`Graphics.Blit(buffer, destination)`方法，把 blur 过后的数据刷到当前需要渲染的显示上。这样的效果就是，当前屏幕，每帧都会经过 blur 处理，然后显示在屏幕上。

```[c#]
int rtW = source.width  / 4;
int rtH = source.height / 4;
RenderTexture buffer = RenderTexture.GetTemporary(rtW, rtH, 0);

// Copy source to the 4x4 smaller texture.
DownSample4x (source, buffer);
```

我们可以看到这个操作，构建了一个比当前截图小四倍的截图数据，可以更节省空间。反正模糊效果就是要看不清楚的。

相应的 shader 如下：

```
Shader "Hidden/BlurAndFlares" {
    Properties {
        _MainTex ("Base (RGB)", 2D) = "" {}
        _NonBlurredTex ("Base (RGB)", 2D) = "" {}
    }

    CGINCLUDE

    #include "UnityCG.cginc"

    struct v2f {
        half4 pos : SV_POSITION;
        half2 uv : TEXCOORD0;
    };

    struct v2f_opts {
        half4 pos : SV_POSITION;
        half2 uv[7] : TEXCOORD0;
    };

    struct v2f_blur {
        half4 pos : SV_POSITION;
        half2 uv : TEXCOORD0;
        half4 uv01 : TEXCOORD1;
        half4 uv23 : TEXCOORD2;
        half4 uv45 : TEXCOORD3;
        half4 uv67 : TEXCOORD4;
    };

    half4 _Offsets;
    half4 _TintColor;

    half _StretchWidth;
    half2 _Threshhold;
    half _Saturation;

    half4 _MainTex_TexelSize;

    sampler2D _MainTex;
    sampler2D _NonBlurredTex;

    v2f vert (appdata_img v) {
        v2f o;
        o.pos = mul(UNITY_MATRIX_MVP, v.vertex);
        o.uv =  v.texcoord.xy;
        return o;
    }

    v2f_blur vertWithMultiCoords2 (appdata_img v) {
        v2f_blur o;
        o.pos = mul(UNITY_MATRIX_MVP, v.vertex);
        o.uv.xy = v.texcoord.xy;
        o.uv01 =  v.texcoord.xyxy + _Offsets.xyxy * half4(1,1, -1,-1);
        o.uv23 =  v.texcoord.xyxy + _Offsets.xyxy * half4(1,1, -1,-1) * 2.0;
        o.uv45 =  v.texcoord.xyxy + _Offsets.xyxy * half4(1,1, -1,-1) * 3.0;
        o.uv67 =  v.texcoord.xyxy + _Offsets.xyxy * half4(1,1, -1,-1) * 4.0;
        o.uv67 =  v.texcoord.xyxy + _Offsets.xyxy * half4(1,1, -1,-1) * 5.0;
        return o;
    }

    v2f_opts vertStretch (appdata_img v) {
        v2f_opts o;
        o.pos = mul(UNITY_MATRIX_MVP, v.vertex);
        half b = _StretchWidth;
        o.uv[0] = v.texcoord.xy;
        o.uv[1] = v.texcoord.xy + b * 2.0 * _Offsets.xy;
        o.uv[2] = v.texcoord.xy - b * 2.0 * _Offsets.xy;
        o.uv[3] = v.texcoord.xy + b * 4.0 * _Offsets.xy;
        o.uv[4] = v.texcoord.xy - b * 4.0 * _Offsets.xy;
        o.uv[5] = v.texcoord.xy + b * 6.0 * _Offsets.xy;
        o.uv[6] = v.texcoord.xy - b * 6.0 * _Offsets.xy;
        return o;
    }

    v2f_opts vertWithMultiCoords (appdata_img v) {
        v2f_opts o;
        o.pos = mul(UNITY_MATRIX_MVP, v.vertex);
        o.uv[0] = v.texcoord.xy;
        o.uv[1] = v.texcoord.xy + 0.5 * _MainTex_TexelSize.xy * _Offsets.xy;
        o.uv[2] = v.texcoord.xy - 0.5 * _MainTex_TexelSize.xy * _Offsets.xy;
        o.uv[3] = v.texcoord.xy + 1.5 * _MainTex_TexelSize.xy * _Offsets.xy;
        o.uv[4] = v.texcoord.xy - 1.5 * _MainTex_TexelSize.xy * _Offsets.xy;
        o.uv[5] = v.texcoord.xy + 2.5 * _MainTex_TexelSize.xy * _Offsets.xy;
        o.uv[6] = v.texcoord.xy - 2.5 * _MainTex_TexelSize.xy * _Offsets.xy;
        return o;
    }

    half4 fragPostNoBlur (v2f i) : SV_Target {
        half4 color = tex2D (_MainTex, i.uv);
        return color * 1.0/(1.0 + Luminance(color.rgb) + 0.5); // this also makes it a little noisy
    }

    half4 fragGaussBlur (v2f_blur i) : SV_Target {
        half4 color = half4 (0,0,0,0);
        color += 0.225 * tex2D (_MainTex, i.uv);
        color += 0.150 * tex2D (_MainTex, i.uv01.xy);
        color += 0.150 * tex2D (_MainTex, i.uv01.zw);
        color += 0.110 * tex2D (_MainTex, i.uv23.xy);
        color += 0.110 * tex2D (_MainTex, i.uv23.zw);
        color += 0.075 * tex2D (_MainTex, i.uv45.xy);
        color += 0.075 * tex2D (_MainTex, i.uv45.zw);
        color += 0.0525 * tex2D (_MainTex, i.uv67.xy);
        color += 0.0525 * tex2D (_MainTex, i.uv67.zw);
        return color;
    }

    half4 fragPreAndCut (v2f_opts i) : SV_Target {
        half4 color = tex2D (_MainTex, i.uv[0]);
        color += tex2D (_MainTex, i.uv[1]);
        color += tex2D (_MainTex, i.uv[2]);
        color += tex2D (_MainTex, i.uv[3]);
        color += tex2D (_MainTex, i.uv[4]);
        color += tex2D (_MainTex, i.uv[5]);
        color += tex2D (_MainTex, i.uv[6]);
        color = max(color / 7.0 - _Threshhold.xxxx, float4(0,0,0,0));
        half lum = Luminance(color.rgb);
        color.rgb = lerp(half3(lum,lum,lum), color.rgb, _Saturation) * _TintColor.rgb;
        return color;
    }

    half4 fragStretch (v2f_opts i) : SV_Target {
        half4 color = tex2D (_MainTex, i.uv[0]);
        color = max (color, tex2D (_MainTex, i.uv[1]));
        color = max (color, tex2D (_MainTex, i.uv[2]));
        color = max (color, tex2D (_MainTex, i.uv[3]));
        color = max (color, tex2D (_MainTex, i.uv[4]));
        color = max (color, tex2D (_MainTex, i.uv[5]));
        color = max (color, tex2D (_MainTex, i.uv[6]));
        return color;
    }

    half4 fragPost (v2f_opts i) : SV_Target {
        half4 color = tex2D (_MainTex, i.uv[0]);
        color += tex2D (_MainTex, i.uv[1]);
        color += tex2D (_MainTex, i.uv[2]);
        color += tex2D (_MainTex, i.uv[3]);
        color += tex2D (_MainTex, i.uv[4]);
        color += tex2D (_MainTex, i.uv[5]);
        color += tex2D (_MainTex, i.uv[6]);
        return color * 1.0/(7.0 + Luminance(color.rgb) + 0.5); // this also makes it a little noisy
    }

    ENDCG

Subshader {
      ZTest Always Cull Off ZWrite Off
      Fog { Mode off }
 Pass {

      CGPROGRAM

      #pragma fragmentoption ARB_precision_hint_fastest
      #pragma exclude_renderers flash
      #pragma vertex vert
      #pragma fragment fragPostNoBlur

      ENDCG
  }

 Pass {

      CGPROGRAM

      #pragma fragmentoption ARB_precision_hint_fastest
      #pragma exclude_renderers flash
      #pragma vertex vertStretch
      #pragma fragment fragStretch

      ENDCG
  }

 // 2
 Pass {

      CGPROGRAM

      #pragma fragmentoption ARB_precision_hint_fastest
      #pragma exclude_renderers flash
      #pragma vertex vertWithMultiCoords
      #pragma fragment fragPreAndCut

      ENDCG
  }

 // 3
 Pass {

      CGPROGRAM

      #pragma fragmentoption ARB_precision_hint_fastest
      #pragma exclude_renderers flash
      #pragma vertex vertWithMultiCoords
      #pragma fragment fragPost

      ENDCG
  }
 // 4
 Pass {

      CGPROGRAM

      #pragma fragmentoption ARB_precision_hint_fastest
      #pragma exclude_renderers flash
      #pragma vertex vertWithMultiCoords2
      #pragma fragment fragGaussBlur

      ENDCG
  }
}

Fallback off

}
```

但是，这样的操作，是整个屏幕都 blur 了，但有时候我们需要高亮一个界面，后面背景动态 blur 如何实现呢。我的实现方法是，在高亮界面打开之前，抓一张屏幕的截图 blur 操作，然后在打开高亮界面，最后把这个 blur 的图片显示在高亮的界面后面做模糊背景。

实现这个过程，需要解决两个问题，保存 blur 后的数据，然后显示这个数据，最后删除这个数据。

```[c#]
public static RawImage rawImage;

// Called by the camera to apply the image effect
void OnRenderImage (RenderTexture source, RenderTexture destination)
{
    if (rawImage != null)
    {
        int rtW = source.width  / 4;
        int rtH = source.height / 4;
        RenderTexture buffer = RenderTexture.GetTemporary(rtW, rtH, 0);

        // Copy source to the 4x4 smaller texture.
        DownSample4x (source, buffer);

        // Blur the small texture
        for(int i = 0; i < iterations; i++)
        {
            RenderTexture buffer2 = RenderTexture.GetTemporary(rtW, rtH, 0);
            FourTapCone (buffer, buffer2, i);
            RenderTexture.ReleaseTemporary(buffer);
            buffer = buffer2;
        }

        // Graphics.Blit(buffer, destination);
        // RenderTexture.ReleaseTemporary(buffer);

        rawImage.texture = buffer;
        this.enabled     = false;
    }
}
```

能够直接显示 RenderTextur e数据的组件是 RawImage，对属性 texture 赋值 RenerTexture 可以直接渲染出来。所以，这里我们利用一个 RawImage，在抓到一帧的数据后先 blur 在保存到 RawImage 里面。

当然了，这个 RawImage 需要外面使用的地方传递给 BlurEffect 组件。然后，里面 enabled false 让 BlurEffect 停止不断的抓当前屏幕的数据。也不调用`Graphics.Blit`去改变当前显示。

然后，这个 RawImage 就可以渲染出当前一个帧模糊的屏幕图片了。切记，最后 RawImage 用完以后，我们需要调用`RenderTexture.ReleaseTemporary((RenderTexture) this.rawImage.texture)`去把 blur 的屏幕截图数据清除。
