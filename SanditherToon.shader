// Copyright (c) 2023 JohnTonarino
// Released under the MIT license
// SanditherToon v 1.5.2
Shader "SanditherToon"
{
    Properties
    {

        [Header(RenderMode)]
        [Space(10)]
        [Enum(UnityEngine.Rendering.CullMode)] _Cull("CullMode", Int) = 2
        [Enum(On, 1, Off, 0)] _DitherFluctuation("DitherFluctuation", Int) = 1
        [Enum(None, 0, RimLightAnimation, 1, MainTexAnimation, 2, DistanceDisappearTex, 3)] _AnimationType("AnimationType", Int) = 0
        [Enum(None, 0, SingleOutline, 1, DoubleOutline, 2)] _OutlineMode("OutlineMode", Int) = 0
        [Enum(Auto, 0, AnimationControl, 1)] _TimeType("TimeType", Int) = 0

        [Header(MainTex)]
        [Space(10)]
        _MainTex("Texture", 2D) = "white" {}
        _MainTexOverlayColor("MainTexOverlayColor", Color) = (1., 1., 1., 1.)
        _MainTexDitherThreshold("MainTexThreshold", Range(0., 1.)) = 0.
        _DisappearTexStart("DissappearTexStart", Float) = 1.
        _DisappearTexEnd("DissappearTexEnd", Float) = .1

        [Header(NormalMap)]
        [Space(10)]
        [Normal]_BumpMap("NormalMap", 2D) = "bump" {}
        _BumpScale("NormalScale", Range(.01, 1.)) = 1.

        [Header(MatCap)]
        [Space(10)]
        _MatCap("MatCap", 2D) = "white" {}
        _MatCapStrength("MatCapStrength", Range(0., 1.)) = 0.
        _MatCapMask("MatCapMask", 2D) = "white" {}

        [Header(Specular)]
        [Space(10)]
        _SpecularStrength("SpecularStrength",Range(0., 1.)) = 0.0
        _SpecularPower("SpecularPower",Range(0.01, 10.)) = 0.01
        _SpecularBias("SpecularBias",Range(0., 1.)) = 0.5

        [Header(Shadow)]
        [Space(10)]
        _ShadowTex("ShadowTex", 2D) = "black" {}
        _ShadowOverlayColor("ShadowOverlayColor", Color) = (0., 0., 0., 1.)
        _ShadowDitherThreshold("ShadowDitherThreshold", Range(.1, 1.)) = .5

        [Header(RimColor)]
        [Space(10)]
        _RimColor("RimLightColor", Color) = (1., 1., 1., 1.)
        _RimLightStrength("RimLightStrength", Range(0., 1.)) = .5
        _RimLightMask("RimLightMask", 2D) = "white" {}
        _RimLightDitherThreshold("RimLightDitherThreshold", Range(.1, 1.)) = .5

        [Header(Outline)]
        [Space(10)]
        _OutlineColor("OutlineColor", Color) = (1., 1., 1., 1.)
        _OuterOutlineColor("OuterOutlineColor", Color) = (1., 1., 1., 1.)
        _AsOutlineUnlit("As Outline Unlit", Range(0,1)) = 0
        _OutlineWidth("OutlineWidth", Range(.0, .05)) = .025
        _OutlineMask("OutlineMask", 2D) = "white" {}

        [Header(Transparent)]
        [Space(10)]
        _TransparentMask("TransparentMask", 2D) = "white" {}
        _TransparentLevel("TransparentLevel", Range(0., 1.)) = 1.

        [Header(Emission)]
        [Space(10)]
        _EmissiveTex("EmissiveTex", 2D) = "black" {}
        [HDR] _EmissiveColor("EmissiveColor", Color) = (1., 1., 1., 1.)

        [Header(Animation)]
        [Space(10)]
        _AnimationSpeed("AnimationSpeed", Range(-1.,1.)) = 0.
        _AnimationTime("AnimationTime", Range(0., 1.)) = 0.

        //------------------------------------------------------------------------------------------------------------------------------
        // [OpenLit] Properties for lighting

        // It is more accurate to set _LightMinLimit to 0, but the avatar will be black.
        // In many cases, setting a small value will give better results.

        [Header(OpenLit)]
        [Space(10)]
        _AsUnlit("As Unlit", Range(0,1)) = 0
        _LightMinLimit("Light Min Limit", Range(0,1)) = 0.05
        _LightMaxLimit("Light Max Limit", Range(0,10)) = 1
        _BeforeExposureLimit("Before Exposure Limit", Float) = 10000
        _MonochromeLighting("Monochrome lighting", Range(0,1)) = 0
        _AlphaBoostFA("Boost Transparency in ForwardAdd", Range(1,100)) = 10
        _LightDirectionOverride("Light Direction Override", Vector) = (0.001,0.002,0.001,0)

        _ShadowThreshold("Shadow Threshold", Range(-1,1)) = 0
        [Toggle(_)] _ReceiveShadow("Receive Shadow", Int) = 0

        //------------------------------------------------------------------------------------------------------------------------------
    }
    SubShader
    {
        LOD 100
        Cull[_Cull]

        CGINCLUDE
        #include "UnityCG.cginc"
        #include "Lighting.cginc"
        #include "AutoLight.cginc"

        //------------------------------------------------------------------------------------------------------------------------------
        // OpenLit Library 1.0.2
        // This code is licensed under CC0 1.0 Universal.
        // https://creativecommons.org/publicdomain/zero/1.0/

        #if !defined(OPENLIT_CORE_INCLUDED)
        #define OPENLIT_CORE_INCLUDED

        //------------------------------------------------------------------------------------------------------------------------------
        // Macro
        #define OPENLIT_LIGHT_COLOR     _LightColor0.rgb
        #define OPENLIT_LIGHT_DIRECTION _WorldSpaceLightPos0.xyz
        #define OPENLIT_MATRIX_M        unity_ObjectToWorld
        #define OPENLIT_FALLBACK_DIRECTION  float4(0.001,0.002,0.001,0)

        //------------------------------------------------------------------------------------------------------------------------------
        // SRGB <-> Linear
        float3 OpenLitLinearToSRGB(float3 col)
        {
            return LinearToGammaSpace(col);
        }

        float3 OpenLitSRGBToLinear(float3 col)
        {
            return GammaToLinearSpace(col);
        }

        //------------------------------------------------------------------------------------------------------------------------------
        // Color
        float OpenLitLuminance(float3 rgb)
        {
            #if defined(UNITY_COLORSPACE_GAMMA)
                return dot(rgb, float3(0.22, 0.707, 0.071));
            #else
                return dot(rgb, float3(0.0396819152, 0.458021790, 0.00609653955));
            #endif
        }

        float OpenLitGray(float3 rgb)
        {
            return dot(rgb, float3(1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0));
        }

        //------------------------------------------------------------------------------------------------------------------------------
        // Structure
        struct OpenLitLightDatas
        {
            float3 lightDirection;
            float3 directLight;
            float3 indirectLight;
        };

        //------------------------------------------------------------------------------------------------------------------------------
        // Light Direction
        // Use `UnityWorldSpaceLightDir(float3 positionWS)` for ForwardAdd passes
        float3 ComputeCustomLightDirection(float4 lightDirectionOverride)
        {
            float3 customDir = length(lightDirectionOverride.xyz) * normalize(mul((float3x3)OPENLIT_MATRIX_M, lightDirectionOverride.xyz));
            return lightDirectionOverride.w ? customDir : lightDirectionOverride.xyz;
        }

        void ComputeLightDirection(out float3 lightDirection, out float3 lightDirectionForSH9, float4 lightDirectionOverride)
        {
            float3 mainDir = OPENLIT_LIGHT_DIRECTION * OpenLitLuminance(OPENLIT_LIGHT_COLOR);
            #if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH
                float3 sh9Dir = unity_SHAr.xyz * 0.333333 + unity_SHAg.xyz * 0.333333 + unity_SHAb.xyz * 0.333333;
                float3 sh9DirAbs = float3(sh9Dir.x, abs(sh9Dir.y), sh9Dir.z);
            #else
                float3 sh9Dir = 0;
                float3 sh9DirAbs = 0;
            #endif
            float3 customDir = ComputeCustomLightDirection(lightDirectionOverride);

            lightDirection = normalize(sh9DirAbs + mainDir + customDir);
            lightDirectionForSH9 = sh9Dir + mainDir;
            lightDirectionForSH9 = dot(lightDirectionForSH9,lightDirectionForSH9) < 0.000001 ? 0 : normalize(lightDirectionForSH9);
        }

        void ComputeLightDirection(out float3 lightDirection, out float3 lightDirectionForSH9)
        {
            ComputeLightDirection(lightDirection, lightDirectionForSH9, OPENLIT_FALLBACK_DIRECTION);
        }

        //------------------------------------------------------------------------------------------------------------------------------
        // ShadeSH9
        void ShadeSH9ToonDouble(float3 lightDirection, out float3 shMax, out float3 shMin)
        {
            #if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH
                float3 N = lightDirection * 0.666666;
                float4 vB = N.xyzz * N.yzzx;
                // L0 L2
                float3 res = float3(unity_SHAr.w,unity_SHAg.w,unity_SHAb.w);
                res.r += dot(unity_SHBr, vB);
                res.g += dot(unity_SHBg, vB);
                res.b += dot(unity_SHBb, vB);
                res += unity_SHC.rgb * (N.x * N.x - N.y * N.y);
                // L1
                float3 l1;
                l1.r = dot(unity_SHAr.rgb, N);
                l1.g = dot(unity_SHAg.rgb, N);
                l1.b = dot(unity_SHAb.rgb, N);
                shMax = res + l1;
                shMin = res - l1;
                #if defined(UNITY_COLORSPACE_GAMMA)
                    shMax = OpenLitLinearToSRGB(shMax);
                    shMin = OpenLitLinearToSRGB(shMin);
                #endif
            #else
                shMax = 0.0;
                shMin = 0.0;
            #endif
        }

        void ShadeSH9ToonDouble(out float3 shMax, out float3 shMin)
        {
            float3 lightDirection, lightDirectionForSH9;
            ComputeLightDirection(lightDirection, lightDirectionForSH9, OPENLIT_FALLBACK_DIRECTION);
            ShadeSH9ToonDouble(lightDirectionForSH9, shMax, shMin);
        }

        float3 ShadeSH9Toon()
        {
            float3 shMax, shMin;
            ShadeSH9ToonDouble(shMax, shMin);
            return shMax;
        }

        float3 ShadeSH9ToonIndirect()
        {
            float3 shMax, shMin;
            ShadeSH9ToonDouble(shMax, shMin);
            return shMin;
        }

        //------------------------------------------------------------------------------------------------------------------------------
        // Lighting
        void ComputeSHLightsAndDirection(out float3 lightDirection, out float3 directLight, out float3 indirectLight, float4 lightDirectionOverride)
        {
            float3 lightDirectionForSH9;
            ComputeLightDirection(lightDirection, lightDirectionForSH9, lightDirectionOverride);
            ShadeSH9ToonDouble(lightDirectionForSH9, directLight, indirectLight);
        }

        void ComputeSHLightsAndDirection(out float3 lightDirection, out float3 directLight, out float3 indirectLight)
        {
            ComputeSHLightsAndDirection(lightDirection, directLight, indirectLight, OPENLIT_FALLBACK_DIRECTION);
        }

        void ComputeLights(out float3 lightDirection, out float3 directLight, out float3 indirectLight, float4 lightDirectionOverride)
        {
            ComputeSHLightsAndDirection(lightDirection, directLight, indirectLight, lightDirectionOverride);
            directLight += OPENLIT_LIGHT_COLOR;
        }

        void ComputeLights(out float3 lightDirection, out float3 directLight, out float3 indirectLight)
        {
            ComputeSHLightsAndDirection(lightDirection, directLight, indirectLight);
            directLight += OPENLIT_LIGHT_COLOR;
        }

        void ComputeLights(out OpenLitLightDatas lightDatas, float4 lightDirectionOverride)
        {
            ComputeLights(lightDatas.lightDirection, lightDatas.directLight, lightDatas.indirectLight, lightDirectionOverride);
        }

        void ComputeLights(out OpenLitLightDatas lightDatas)
        {
            ComputeLights(lightDatas.lightDirection, lightDatas.directLight, lightDatas.indirectLight);
        }

        //------------------------------------------------------------------------------------------------------------------------------
        // Correct
        void CorrectLights(inout OpenLitLightDatas lightDatas, float lightMinLimit, float lightMaxLimit, float monochromeLighting, float asUnlit)
        {
            lightDatas.directLight = clamp(lightDatas.directLight, lightMinLimit, lightMaxLimit);
            lightDatas.directLight = lerp(lightDatas.directLight, OpenLitGray(lightDatas.directLight), monochromeLighting);
            lightDatas.directLight = lerp(lightDatas.directLight, 1.0, asUnlit);
            lightDatas.indirectLight = clamp(lightDatas.indirectLight, 0.0, lightMaxLimit);
        }

        //------------------------------------------------------------------------------------------------------------------------------
        // Vertex Lighting
        float3 ComputeAdditionalLights(float3 positionWS, float3 positionCS)
        {
            float4 toLightX = unity_4LightPosX0 - positionWS.x;
            float4 toLightY = unity_4LightPosY0 - positionWS.y;
            float4 toLightZ = unity_4LightPosZ0 - positionWS.z;

            float4 lengthSq = toLightX * toLightX + 0.000001;
            lengthSq += toLightY * toLightY;
            lengthSq += toLightZ * toLightZ;

            //float4 atten = 1.0 / (1.0 + lengthSq * unity_4LightAtten0);
            float4 atten = saturate(saturate((25.0 - lengthSq * unity_4LightAtten0) * 0.111375) / (0.987725 + lengthSq * unity_4LightAtten0));

            float3 additionalLightColor;
            additionalLightColor = unity_LightColor[0].rgb * atten.x;
            additionalLightColor = additionalLightColor + unity_LightColor[1].rgb * atten.y;
            additionalLightColor = additionalLightColor + unity_LightColor[2].rgb * atten.z;
            additionalLightColor = additionalLightColor + unity_LightColor[3].rgb * atten.w;

            return additionalLightColor;
        }

        //------------------------------------------------------------------------------------------------------------------------------
        // Encode and decode
        #if !defined(SHADER_API_GLES)
        // -1 - 1
        uint EncodeNormalizedFloat3ToUint(float3 vec)
        {
            uint valx = abs(vec.x) >= 1 ? 511 : abs(vec.x) * 511;
            uint valy = abs(vec.y) >= 1 ? 511 : abs(vec.y) * 511;
            uint valz = abs(vec.z) >= 1 ? 511 : abs(vec.z) * 511;
            valx = valx & 0x000001ffu;
            valy = valy & 0x000001ffu;
            valz = valz & 0x000001ffu;
            valx += vec.x > 0 ? 0 : 512;
            valy += vec.y > 0 ? 0 : 512;
            valz += vec.z > 0 ? 0 : 512;

            valy = valy << 10;
            valz = valz << 20;
            return valx | valy | valz;
        }

        float3 DecodeNormalizedFloat3FromUint(uint val)
        {
            // 5 math in target 5.0
            uint3 val3 = val >> uint3(0,10,20);
            float3 vec = val3 & 0x000001ffu;
            vec /= (val3 & 0x00000200u) == 0x00000200u ? -511.0 : 511.0;
            return vec;
        }

        // 0 - 999
        uint EncodeHDRColorToUint(float3 col)
        {
            col = clamp(col, 0, 999);
            float maxcol = max(col.r,max(col.g,col.b));

            float floatDigit = maxcol == 0 ? 0 : log10(maxcol);
            uint digit = floatDigit >= 0 ? floatDigit + 1 : 0;
            if (digit > 3) digit = 3;
            float scale = pow(10,digit);
            col /= scale;

            uint R = col.r * 1023;
            uint G = col.g * 1023;
            uint B = col.b * 1023;
            uint M = digit;
            R = R & 0x000003ffu;
            G = G & 0x000003ffu;
            B = B & 0x000003ffu;

            G = G << 10;
            B = B << 20;
            M = M << 30;
            return R | G | B | M;
        }

        float3 DecodeHDRColorFromUint(uint val)
        {
            // 5 math in target 5.0
            uint4 RGBM = val >> uint4(0,10,20,30);
            return float3(RGBM.rgb & 0x000003ffu) / 1023.0 * pow(10,RGBM.a);
        }

        void PackLightDatas(out uint3 pack, OpenLitLightDatas lightDatas)
        {
            pack = uint3(
                EncodeNormalizedFloat3ToUint(lightDatas.lightDirection),
                EncodeHDRColorToUint(lightDatas.directLight),
                EncodeHDRColorToUint(lightDatas.indirectLight)
            );
        }

        void UnpackLightDatas(out OpenLitLightDatas lightDatas, uint3 pack)
        {
            lightDatas.lightDirection = DecodeNormalizedFloat3FromUint(pack.x);
            lightDatas.directLight = DecodeHDRColorFromUint(pack.y);
            lightDatas.indirectLight = DecodeHDRColorFromUint(pack.z);
        }
        #endif // #if !defined(SHADER_API_GLES)
        #endif // #if !defined(OPENLIT_CORE_INCLUDED)
        // OpenLit
        //------------------------------------------------------------------------------------------------------------------------------


        #pragma skip_variants LIGHTMAP_ON DYNAMICLIGHTMAP_ON LIGHTMAP_SHADOW_MIXING SHADOWS_SHADOWMASK DIRLIGHTMAP_COMBINED
        #define PI 3.141592

        struct appdata {
            float4 vertex : POSITION;
            float2 uv : TEXCOORD0;
            float2 uv1 : TEXCOORD1;
            half3 normalOS : NORMAL;
            half4 tangent : TANGENT;
            UNITY_VERTEX_INPUT_INSTANCE_ID
        };

        struct v2f_sndbase {
            float4 pos : SV_POSITION;
            float3 positionWS : TEXCOORD0;
            float2 uv : TEXCOORD1;
            float3 normalWS : TEXCOORD2;

            // [OpenLit] Add light datas
            nointerpolation uint3 lightDatas : TEXCOORD3;
            UNITY_FOG_COORDS(4)
                UNITY_LIGHTING_COORDS(5, 6)
#if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH
                float3 vertexLight  : TEXCOORD7;
#endif
            UNITY_VERTEX_OUTPUT_STEREO

            float4 screenPos : TEXCOORD8;
            half3 tangent : TEXCOORD9;
            half3 binormal : TEXCOORD10;
            half2 viewUV : TEXCOORD11;
        };

        static const int pattern[4][4] = {
            { 1, 13,  4, 16},
            { 9,  5, 12,  8},
            { 3, 15,  2, 14},
            {11,  7, 10,  6}
        };

        sampler2D _MainTex;
        float4 _MainTex_ST;
        fixed4 _MainTexOverlayColor;
        half _MainTexDitherThreshold;
        half _DisappearTexStart;
        half _DisappearTexEnd;

        half _BumpScale;
        sampler2D _BumpMap;
        float4 _BumpMap_ST;

        sampler2D _MatCap;
        half _MatCapStrength;
        sampler2D _MatCapMask;

        half _SpecularStrength;
        half _SpecularPower;
        half _SpecularBias;

        sampler2D _ShadowTex;
        fixed4 _ShadowOverlayColor;
        half _ShadowDitherThreshold;

        fixed4 _RimColor;
        half _RimLightStrength;
        sampler2D _RimLightMask;
        half _RimLightDitherThreshold;
        float _AnimationSpeed;
        float _AnimationTime;

        fixed4 _OutlineColor;
        fixed4 _OuterOutlineColor;
        float  _AsOutlineUnlit;
        half   _OutlineWidth;
        sampler2D _OutlineMask;

        sampler2D _TransparentMask;
        half _TransparentLevel;

        sampler2D _EmissiveTex;
        float4 _EmissiveColor;

        half _DitherThreshold;

        // [OpenLit] Properties for lighting
        float _LightIntensity;
        uint _ReceiveShadow;

        float   _AsUnlit;
        float   _LightMinLimit;
        float   _LightMaxLimit;
        float   _BeforeExposureLimit;
        float   _MonochromeLighting;
        float   _AlphaBoostFA;
        float4  _LightDirectionOverride;

        float _ShadowThreshold;
        //---
        uint _DitherFluctuation;
        uint _AnimationType;
        uint _OutlineMode;
        uint _TimeType;

        float rand2(float2 seed) {
            return frac(sin(dot(seed, float2(12.9898, 78.233))) * 43758.5453);
        }

        fixed calcDitheringPattern(float2 screenPos) {
            int2 patternIndexVec;
            if (_DitherFluctuation == 1) {
                patternIndexVec = int2(fmod(screenPos + rand2(fmod(float2(_Time.x, _Time.y), 10.) + float2(.01, .0)), 4.));
            }
            else {
                patternIndexVec = int2(fmod(screenPos, 4.));
            }
            return 1. / 17. * (float)pattern[patternIndexVec.y][patternIndexVec.x];
        }
        fixed drawShadowPattern(float2 INuv, float4 INscreenPos) {
            float2 viewportPos = INscreenPos.xy / INscreenPos.w;
            float2 screenPos = viewportPos * _ScreenParams.xy;

            fixed shadowPattern = calcDitheringPattern(screenPos) > _ShadowDitherThreshold ? 1. : 0.;

            return shadowPattern;
        }
        fixed drawRimLightPattern(float2 INuv, float4 INscreenPos, float3 viewDir, float3 INnormal) {
            float2 viewportPos = INscreenPos.xy / INscreenPos.w;
            float2 screenPos = viewportPos * _ScreenParams.xy;
            fixed4 rimLightMask = tex2D(_RimLightMask, INuv);
            float rim = lerp(0., pow(1. - saturate(dot(viewDir, INnormal)), 2.), _RimLightStrength) * rimLightMask.x;

            float t = _TimeType == 0 ? _Time.y : _AnimationTime;
            float rad = _AnimationSpeed * t * PI;
            float clippingStrength = _AnimationType == 1 ? min(1., sin(rad) * sin(rad) + .1) : .1;

            fixed rimLightPattern = calcDitheringPattern(screenPos) > _RimLightDitherThreshold ? 1. : 0.;
            return rimLightPattern * rim - clippingStrength;
        }
        fixed drawMainTexPattern(float2 INuv, float4 INscreenPos) {
            float2 viewportPos = INscreenPos.xy / INscreenPos.w;
            float2 screenPos = viewportPos * _ScreenParams.xy;

            float t = _TimeType == 0 ? _Time.y : _AnimationTime;
            float rad = (INuv.y + _AnimationSpeed * t *PI)*.5;
            float distance = length(float3(UNITY_MATRIX_MV[0][3], UNITY_MATRIX_MV[1][3], UNITY_MATRIX_MV[2][3]));

            float clippingStrength;
            if (_AnimationType == 2) {
                clippingStrength = sin(rad) * sin(rad);
            }
            else if (_AnimationType == 3) {
                clippingStrength = 1. - min(_DisappearTexStart, max(distance - _DisappearTexEnd, 0.)) / _DisappearTexStart;
            }
            else {
                clippingStrength = 1.;
            }

            return calcDitheringPattern(screenPos)-_MainTexDitherThreshold*clippingStrength;
        }

        v2f_sndbase vert_snd(appdata v) {
            v2f_sndbase o;
            UNITY_INITIALIZE_OUTPUT(v2f_sndbase, o);
            UNITY_SETUP_INSTANCE_ID(v);
            UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

            o.pos = UnityObjectToClipPos(v.vertex);
            o.positionWS = mul(unity_ObjectToWorld, float4(v.vertex.xyz, 1.));
            o.uv = v.uv;
            o.normalWS = UnityObjectToWorldNormal(v.normalOS);

            o.screenPos = ComputeScreenPos(o.pos);
            o.tangent = normalize(mul(unity_ObjectToWorld, v.tangent)).xyz;
            o.binormal = normalize(mul(unity_ObjectToWorld, cross(v.normalOS, v.tangent) * v.tangent.w));

            float3 viewNormal = mul((float3x3)UNITY_MATRIX_V, UnityObjectToWorldNormal(v.normalOS));
            o.viewUV = viewNormal.xy * .5 + .5;

            UNITY_TRANSFER_FOG(o, o.pos);
            UNITY_TRANSFER_LIGHTING(o, v.uv);

            // [OpenLit] Calculate and copy vertex lighting
#if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH && defined(VERTEXLIGHT_ON)
            o.vertexLight = 0.;
            o.vertexLight = min(o.vertexLight, _LightMaxLimit);
#endif

            // [OpenLit] Calculate and copy light datas
            OpenLitLightDatas lightDatas;
            ComputeLights(lightDatas, _LightDirectionOverride);
            CorrectLights(lightDatas, _LightMinLimit, _LightMaxLimit, _MonochromeLighting, _AsUnlit);
            PackLightDatas(o.lightDatas, lightDatas);

            return o;
        }
        ENDCG
        // For ForwardBase Light
        Pass
        {
            Tags {"LightMode" = "ForwardBase"}
            BlendOp Add, Add
            Blend SrcAlpha OneMinusSrcAlpha

            CGPROGRAM
            #pragma vertex vert_snd
            #pragma fragment frag
            #pragma multi_compile_fwdbase
            #pragma multi_compile_fog

            fixed4 frag(v2f_sndbase i) : SV_Target
            {
                UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
                UNITY_LIGHT_ATTENUATION(attenuation, i, i.positionWS);

                clip(drawMainTexPattern(i.uv, i.screenPos));

                float3 viewDir = normalize(_WorldSpaceCameraPos.xyz-i.positionWS);

                // Lighting
                // [OpenLit] Copy light datas from the input
                OpenLitLightDatas lightDatas;
                UnpackLightDatas(lightDatas, i.lightDatas);

                half3 normalmap = UnpackScaleNormal(tex2D(_BumpMap, i.uv), _BumpScale);
                float3 N = (i.tangent * normalmap.x) + (i.binormal * normalmap.y) + (i.normalWS * normalmap.z);
                float3 L = lightDatas.lightDirection;
                float NdotL = dot(N, L);

                float3 H = normalize(L + viewDir);
                half phoneSpec = pow(smoothstep(_SpecularBias-.02,_SpecularBias+.02,max(0., dot(N,H))), _SpecularPower);

                fixed4 shadowTexColor = tex2D(_ShadowTex, i.uv);
                fixed4 shadowColor = drawShadowPattern(i.uv, i.screenPos) > 0. ? shadowTexColor * _ShadowOverlayColor : 1.;
                float3 factor = NdotL > _ShadowThreshold ? 1 : shadowColor.rgb;
                if (_ReceiveShadow) factor *= attenuation;

                fixed4 col = tex2D(_MainTex, i.uv) * _MainTexOverlayColor;
                fixed4 matcap = tex2D(_MatCap, i.viewUV) * tex2D(_MatCapMask, i.uv);
                col.rgb = lerp(col.rgb, matcap.rgb, _MatCapStrength);

                col = drawRimLightPattern(i.uv, i.screenPos, viewDir, i.normalWS) > 0. ? _RimColor : col;

                fixed4 alphaMask = tex2D(_TransparentMask, i.uv);
                col.a = col.a * OpenLitGray(alphaMask.rgb);
                if (col.a < _TransparentLevel) discard;

                fixed4 emissiveTex = tex2D(_EmissiveTex, i.uv);
                col.rgb += emissiveTex.rgb * _EmissiveColor;

                col.rgb *= lerp(lightDatas.indirectLight, lightDatas.directLight, factor);
                col.rgb *= lerp(1., 1.+phoneSpec, _SpecularStrength);

                fixed3 albedo = col.rgb;
#if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH
                col.rgb += albedo * i.vertexLight;
                col.rgb = min(col.rgb, albedo.rgb * _LightMaxLimit);
#endif
                UNITY_APPLY_FOG(i.fogCoord, col);

                return col;
            }
            ENDCG
        }
        // For ForwardAdd Light
        Pass
        {
            Tags { "LightMode" = "ForwardAdd"}

            // [OpenLit] ForwardAdd uses "BlendOp Max" to avoid overexposure
            BlendOp Max, Add
            Blend One One, Zero One

            CGPROGRAM
            #pragma vertex vert_snd
            #pragma fragment frag
            #pragma multi_compile_fwdadd
            #pragma multi_compile_fog

            fixed4 frag(v2f_sndbase i) :SV_Target
            {
                UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
                UNITY_LIGHT_ATTENUATION(attenuation, i, i.positionWS);

                clip(drawMainTexPattern(i.uv, i.screenPos));

                float3 viewDir = normalize(_WorldSpaceCameraPos.xyz - i.positionWS);

                // Lighting
                // [OpenLit] Copy light datas from the input
                OpenLitLightDatas lightDatas;
                UnpackLightDatas(lightDatas, i.lightDatas);

                half3 normalmap = UnpackScaleNormal(tex2D(_BumpMap, i.uv), _BumpScale);
                float3 N = (i.tangent * normalmap.x) + (i.binormal * normalmap.y) + (i.normalWS * normalmap.z);
                float3 L = lightDatas.lightDirection;
                float NdotL = dot(N, L);

                half3 H = normalize(L + viewDir);
                half phoneSpec = pow(smoothstep(_SpecularBias-.02,_SpecularBias+.02,max(0., (dot(N, H)))), _SpecularPower);

                fixed4 shadowTexColor = tex2D(_ShadowTex, i.uv);
                fixed4 shadowColor = drawShadowPattern(i.uv, i.screenPos) > 0. ? shadowTexColor * _ShadowOverlayColor : 1.;
                float3 factor = NdotL > _ShadowThreshold ? 1 : shadowColor.rgb;

                fixed4 col = tex2D(_MainTex, i.uv) * _MainTexOverlayColor;
                fixed4 matcap = tex2D(_MatCap, i.viewUV) * tex2D(_MatCapMask, i.uv);
                col.rgb = lerp(col.rgb, matcap.rgb, _MatCapStrength);

                col = drawRimLightPattern(i.uv, i.screenPos, viewDir, i.normalWS) > 0. ? _RimColor : col;

                fixed4 alphaMask = tex2D(_TransparentMask, i.uv);
                col.a = col.a * OpenLitGray(alphaMask.rgb);
                if (col.a < _TransparentLevel) discard;

                fixed4 emissiveTex = tex2D(_EmissiveTex, i.uv);
                col.rgb += emissiveTex.rgb * _EmissiveColor;

                col.rgb *= lerp(0., OPENLIT_LIGHT_COLOR, factor*attenuation);
                col.rgb *= lerp(1., 1.+phoneSpec, _SpecularStrength);

                UNITY_APPLY_FOG(i.fogCoord, col);

                // [OpenLit] Premultiply (only for transparent materials)
                col.rgb *= saturate(col.a * _AlphaBoostFA);

                return col;
            }
            ENDCG
        }
        // For Outline
        Pass
        {
            Tags {"LightMode" = "ForwardBase"}
            BlendOp Add, Add
            Blend SrcAlpha OneMinusSrcAlpha
            Cull Front

            CGPROGRAM
            #pragma vertex vert
            #pragma geometry geom
            #pragma fragment frag

            struct g2f {
                float4 pos : SV_POSITION;
                float3 positionWS : TEXCOORD0;
                float2 uv : TEXCOORD1;

                // [OpenLit] Add light datas
                nointerpolation uint3 lightDatas : TEXCOORD3;
                UNITY_FOG_COORDS(4)
                UNITY_LIGHTING_COORDS(5, 6)
#if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH
                float3 vertexLight  : TEXCOORD7;
    #endif
                UNITY_VERTEX_OUTPUT_STEREO

                float4 screenPos : TEXCOORD8;
                half2 viewUV : TEXCOORD9;
                fixed4 color : COLOR;
            };

            appdata vert(appdata v) {
                return v;
            }

            [maxvertexcount(6)]
            void geom(triangle appdata IN[3], inout TriangleStream<g2f> stream) {
                g2f o;
                UNITY_INITIALIZE_OUTPUT(g2f, o);
                UNITY_SETUP_INSTANCE_ID(IN[0]);
                UNITY_SETUP_INSTANCE_ID(IN[1]);
                UNITY_SETUP_INSTANCE_ID(IN[2]);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

                float2 offsets[3];
                for (int i = 0; i < 3; ++i) {
                    appdata v = IN[i];
                    fixed4 outlineMask = tex2Dlod(_OutlineMask, float4(v.uv.xy, 0., 0.));
                    float3 norm = normalize(mul((float3x3)UNITY_MATRIX_IT_MV, v.normalOS));
                    offsets[i] = TransformViewToProjection(norm.xy)*outlineMask.r;
                }

                // 1st Outline
                for (int i = 0; i < 3; ++i) {
                    appdata v = IN[i];
                    v2f_sndbase sndbase = vert_snd(v);

                    o.pos = sndbase.pos;
                    o.pos.xy += _OutlineMode == 0 ? 0. : (_OutlineMode == 1 ?  offsets[i] * _OutlineWidth : .5 * offsets[i] * _OutlineWidth);
                    o.positionWS = sndbase.positionWS;
                    o.uv = sndbase.uv;

                    o.screenPos = sndbase.screenPos;

                    o.viewUV = sndbase.viewUV;

                    UNITY_TRANSFER_FOG(o, o.pos);
                    UNITY_TRANSFER_LIGHTING(o, sndbase.uv);

                    // [OpenLit] Calculate and copy vertex lighting
#if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH && defined(VERTEXLIGHT_ON)
                    o.vertexLight = 0.;
                    o.vertexLight = min(o.vertexLight, _LightMaxLimit);
#endif
                    o.color = _OutlineColor;

                    // [OpenLit] Calculate and copy light datas
                    OpenLitLightDatas lightDatas;
                    ComputeLights(lightDatas, _LightDirectionOverride);
                    CorrectLights(lightDatas, _LightMinLimit, _LightMaxLimit, _MonochromeLighting, _AsOutlineUnlit);
                    PackLightDatas(o.lightDatas, lightDatas);

                    stream.Append(o);
                }
                stream.RestartStrip();

                // 2nd Outline
                for (int i = 0; i < 3; ++i) {
                    appdata v = IN[i];

                    v2f_sndbase sndbase = vert_snd(v);

                    o.pos = sndbase.pos;
                    o.pos.xy += _OutlineMode == 2 ?  offsets[i] * _OutlineWidth : 0.;
                    o.positionWS = sndbase.positionWS;
                    o.uv = sndbase.uv;

                    o.screenPos = sndbase.screenPos;

                    o.viewUV = sndbase.viewUV;

                    UNITY_TRANSFER_FOG(o, o.pos);
                    UNITY_TRANSFER_LIGHTING(o, sndbase.uv);

                    // [OpenLit] Calculate and copy vertex lighting
#if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH && defined(VERTEXLIGHT_ON)
                    o.vertexLight = 0.;
                    o.vertexLight = min(o.vertexLight, _LightMaxLimit);
#endif
                    o.color = _OuterOutlineColor;

                    // [OpenLit] Calculate and copy light datas
                    OpenLitLightDatas lightDatas;
                    ComputeLights(lightDatas, _LightDirectionOverride);
                    CorrectLights(lightDatas, _LightMinLimit, _LightMaxLimit, _MonochromeLighting, _AsOutlineUnlit);
                    PackLightDatas(o.lightDatas, lightDatas);

                    stream.Append(o);
                }
                stream.RestartStrip();
            }
            fixed4 frag(g2f i) :SV_Target
            {
                UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
                UNITY_LIGHT_ATTENUATION(attenuation, i, i.positionWS);

                clip(drawMainTexPattern(i.uv, i.screenPos));

                // Lighting
                // [OpenLit] Copy light datas from the input
                OpenLitLightDatas lightDatas;
                UnpackLightDatas(lightDatas, i.lightDatas);

                float factor = 1.;
                if (_ReceiveShadow) factor *= attenuation;
                if (_OutlineMode == 0)discard;

                fixed4 col = i.color;

                col.rgb *= lerp(lightDatas.indirectLight, lightDatas.directLight, factor);
                fixed3 albedo = col.rgb;
#if !defined(LIGHTMAP_ON) && UNITY_SHOULD_SAMPLE_SH
                col.rgb += albedo * i.vertexLight;
                col.rgb = min(col.rgb, albedo.rgb * _LightMaxLimit);
#endif
                UNITY_APPLY_FOG(i.fogCoord, col);

                return col;
            }
            ENDCG
        }
        // For ShadowRendering
        Pass
        {
            Tags {"LightMode" = "ShadowCaster"}

            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #pragma multi_compile_shadowcaster
            #include "UnityCG.cginc"

            struct v2f_shadow {
                V2F_SHADOW_CASTER;
                float2 uv : TEXCOORD1;
                float4 screenPos : TEXCOORD2;
            };

            v2f_shadow vert(appdata_base v)
            {
                v2f_shadow o;
                TRANSFER_SHADOW_CASTER_NORMALOFFSET(o)
                o.pos = UnityObjectToClipPos(v.vertex);
                o.uv = v.texcoord.xy;
                o.screenPos = ComputeScreenPos(o.pos);
                return o;
            }
            float4 frag(v2f_shadow i) : SV_Target
            {
                clip(drawMainTexPattern(i.uv, i.screenPos));
                SHADOW_CASTER_FRAGMENT(i)
            }
            ENDCG
        }
    }
}
