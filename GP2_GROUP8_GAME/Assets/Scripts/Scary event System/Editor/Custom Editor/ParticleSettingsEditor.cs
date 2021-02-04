using Scary_event_System.Other;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Editor.Custom_Editor
{
    [CustomEditor(typeof(ParticleSettings))]
    public class ParticleSettingsEditor : UnityEditor.Editor
    {
        private ParticleSystem _particleSystem;
        private ParticleSettings _settings = null;

        private static void ClearParticle(ParticleSystem ps)
        {
            ps.Stop();
            ps.Clear();
        }

        public override void OnInspectorGUI()
        {
            if (_settings == null || _particleSystem == null)
            {
                _settings = (ParticleSettings) target;
                _particleSystem = _settings.gameObject.GetComponent<ParticleSystem>();
            }

            if (_settings == null || _particleSystem == null) return;

            var main = _particleSystem.main;


            EditorGUI.BeginChangeCheck();
            main.startRotationZ = EditorGUILayout.Slider("Rotation: ", main.startRotationZ.constant, 0f, 360);

            if (EditorGUI.EndChangeCheck())
            {
                ClearParticle(_particleSystem);
                _particleSystem.Play();
            }

            EditorGUI.BeginChangeCheck();
            main.startSizeX = EditorGUILayout.Slider("Size: ", main.startSizeX.constant, 0.5f, 3.5f);

            if (EditorGUI.EndChangeCheck())
            {
                ClearParticle(_particleSystem);
                _particleSystem.Play();
            }
        }
    }
}