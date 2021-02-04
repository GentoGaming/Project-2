using Scary_event_System.ScaryEventUI;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Editor.Custom_Editor
{
    [CustomEditor(typeof(ParticleScaryEvent))]
    public class ParticleEditorScaryEvent : UnityEditor.Editor
    {
        private bool _isEditing;
        private string _prefabName = "";
        private string _transformName = "";

        public override void OnInspectorGUI()
        {
            base.OnInspectorGUI();
            EditorGUILayout.LabelField(
                "--------------------------------------------------------------------------------");
            EditorGUILayout.LabelField("Add new Particle Event");
            ParticleScaryEvent scrayEvent = (ParticleScaryEvent) target;

            if (scrayEvent.transform != null)
            {
                _isEditing = true;
            }

            EditorGUI.BeginChangeCheck();
            scrayEvent.delay = EditorGUILayout.Slider("Particle Delay", scrayEvent.delay, 0, 50);
            EditorGUI.EndChangeCheck();


            if (!_isEditing)
            {
                EditorGUILayout.LabelField(" Choose the GameObject where you Prefab Particle Will be instantiated ");
                EditorGUI.BeginChangeCheck();
                scrayEvent.objTransform = (Transform) EditorGUILayout.ObjectField("Specify game object",
                    scrayEvent.objTransform, typeof(GameObject), true);
                if (EditorGUI.EndChangeCheck() && scrayEvent.objTransform != null)
                {
                    _isEditing = true;
                }
            }


            if (_isEditing)
            {
                EditorGUI.BeginChangeCheck();
                scrayEvent.objTransform = (Transform) EditorGUILayout.ObjectField("Specify game object",
                    scrayEvent.objTransform, typeof(Transform), true);
                if (EditorGUI.EndChangeCheck() && scrayEvent.objTransform == null)
                {
                    _isEditing = false;
                }

                EditorGUI.BeginChangeCheck();

                scrayEvent.prefab = (GameObject) EditorGUILayout.ObjectField("Particle prefab", scrayEvent.prefab,
                    typeof(GameObject), false);
                EditorGUI.EndChangeCheck();

                if (scrayEvent.objTransform != null && scrayEvent.prefab != null)
                {
                    if (GUILayout.Button("Save"))
                    {
                        scrayEvent.AddCustomParticle(scrayEvent.objTransform, scrayEvent.prefab, scrayEvent.delay);
                        scrayEvent.objTransform = null;
                        scrayEvent.prefab = null;
                        scrayEvent.delay = 0;
                        _isEditing = false;
                    }
                }
            }


            EditorGUILayout.LabelField(
                "--------------------------------------------------------------------------------");
            EditorGUILayout.LabelField("All Saved Prefabs Events");
            foreach (var customParticle in scrayEvent.particles)
            {
                GUILayout.BeginHorizontal();
                _transformName = customParticle.objTransform.name;
                _prefabName = customParticle.prefab.name;
                if (_transformName.Length > 10)
                {
                    _transformName = _transformName.Substring(0, 10) + "...";
                }

                if (_prefabName.Length > 10)
                {
                    _prefabName = _prefabName.Substring(0, 10) + "...";
                }

                EditorGUILayout.LabelField(_transformName + ", SoundClip: " + _prefabName + ", Delayed: " +
                                           customParticle.delay);
                if (GUILayout.Button("Deleted"))
                {
                    scrayEvent.DeleteParticle(customParticle);
                }

                GUILayout.EndHorizontal();
            }
        }
    }
}