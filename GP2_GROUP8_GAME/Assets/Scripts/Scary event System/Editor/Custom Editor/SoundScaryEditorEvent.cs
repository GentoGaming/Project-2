using Scary_event_System.ScaryEventUI;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Editor.Custom_Editor
{
    [CustomEditor(typeof(SoundScaryEvent))]
    public class SoundScaryEditorEvent : UnityEditor.Editor
    {
        private string _audioClipName = "";
        private string _gameObjectName = "";
        private bool _isEditing;

        public override void OnInspectorGUI()
        {
            base.OnInspectorGUI();
            EditorGUILayout.LabelField(
                "--------------------------------------------------------------------------------");
            EditorGUILayout.LabelField("Add new Sound Event");

            SoundScaryEvent scaryEvent = (SoundScaryEvent) target;
            if (scaryEvent == null) return;


            if (scaryEvent.gameObj != null)
            {
                _isEditing = true;
            }

            EditorGUI.BeginChangeCheck();
            scaryEvent.delaySound = EditorGUILayout.Slider("Sound Delay", scaryEvent.delaySound, 0, 50);
            EditorGUI.EndChangeCheck();

            if (!_isEditing)
            {
                EditorGUILayout.LabelField(" Choose the GameObject where you want the audio to come from ");
                EditorGUI.BeginChangeCheck();
                scaryEvent.gameObj = (GameObject) EditorGUILayout.ObjectField("Specify game object", scaryEvent.gameObj,
                    typeof(GameObject), true);
                if (EditorGUI.EndChangeCheck() && scaryEvent.gameObj != null)
                {
                    _isEditing = true;
                }
            }

            if (_isEditing)
            {
                EditorGUI.BeginChangeCheck();
                scaryEvent.gameObj =
                    (GameObject) EditorGUILayout.ObjectField("Sound Clip", scaryEvent.gameObj, typeof(GameObject),
                        true);
                if (EditorGUI.EndChangeCheck() && scaryEvent.gameObj == null)
                {
                    _isEditing = false;
                }

                EditorGUI.BeginChangeCheck();

                scaryEvent.audioClip =
                    (AudioClip) EditorGUILayout.ObjectField("Sound Clip", scaryEvent.audioClip, typeof(AudioClip),
                        false);
                EditorGUI.EndChangeCheck();

                if (scaryEvent.gameObj != null && scaryEvent.audioClip != null)
                {
                    if (GUILayout.Button("Save"))
                    {
                        scaryEvent.AddCustomSound(scaryEvent.gameObj, scaryEvent.audioClip, scaryEvent.delaySound);
                        scaryEvent.gameObj = null;
                        scaryEvent.audioClip = null;
                        scaryEvent.delaySound = 0;
                        _isEditing = false;
                    }
                }
            }

            EditorGUILayout.LabelField(
                "--------------------------------------------------------------------------------");
            EditorGUILayout.LabelField("All Saved Sound Events");
            foreach (var sound in scaryEvent.soundClasses)
            {
                GUILayout.BeginHorizontal();
                _gameObjectName = sound.gameObj.name;
                _audioClipName = sound.audioClip.name;
                if (_gameObjectName.Length > 10)
                {
                    _gameObjectName = _gameObjectName.Substring(0, 10) + "...";
                }

                if (_audioClipName.Length > 10)
                {
                    _audioClipName = _audioClipName.Substring(0, 10) + "...";
                }

                EditorGUILayout.LabelField(_gameObjectName + ", SoundClip: " + _audioClipName + ", Delayed: " +
                                           sound.delaySound);
                if (GUILayout.Button("Deleted"))
                {
                    scaryEvent.DeleteSound(sound);
                }

                GUILayout.EndHorizontal();
            }
        }
    }
}