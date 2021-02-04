using Scary_event_System.ScaryEventUI;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Editor.Custom_Editor
{
    [CustomEditor(typeof(PosRotScaryEvent))]
    public class PosRotEditorScaryEvent : UnityEditor.Editor
    {
        private bool _isEditing = false;
        private bool _needsReset = false;
        private PosRotScaryEvent _posRotScaryEvent = null;


        // UI Buttons 
        private readonly string _resetButtonText = "reset Position";
        private readonly string _startButtonText = "Start";
        private readonly string _targetButtonText = "target: ";
        private readonly string _finishButtonText = "Finish";
        
        public override void OnInspectorGUI()
        {
            if (_posRotScaryEvent == null)
            {
                _posRotScaryEvent = (PosRotScaryEvent) target;
            }

            if (_posRotScaryEvent == null) return;


            if (!_isEditing && _posRotScaryEvent.target != null)
            {
                _needsReset = _posRotScaryEvent.target.localPosition != _posRotScaryEvent.startPosition ||
                              _posRotScaryEvent.target.localRotation.eulerAngles != _posRotScaryEvent.startRotation;

                if (_needsReset)
                {
                    if (GUILayout.Button(_resetButtonText))
                    {
                        _posRotScaryEvent.target.localPosition = _posRotScaryEvent.startPosition;
                        _posRotScaryEvent.target.localRotation = Quaternion.Euler(_posRotScaryEvent.startRotation);
                        _needsReset = !_needsReset;
                    }
                }
            }


            if (!_isEditing && _posRotScaryEvent.target != null && !_needsReset)
            {
                Transform targetTransform = _posRotScaryEvent.target.transform;
                if (GUILayout.Button(_startButtonText))
                {
                    ActiveEditorTracker.sharedTracker.isLocked = true;
                    _isEditing = true;
                    _posRotScaryEvent.startPosition = targetTransform.localPosition;
                    _posRotScaryEvent.startRotation = targetTransform.localRotation.eulerAngles;
                }
            }

            base.OnInspectorGUI();

            EditorGUI.BeginChangeCheck();
            _posRotScaryEvent.target =
                (Transform) EditorGUILayout.ObjectField(_targetButtonText, _posRotScaryEvent.target, typeof(Transform), true);
            if (EditorGUI.EndChangeCheck() && _posRotScaryEvent.target != null)
            {
                _posRotScaryEvent.startPosition = _posRotScaryEvent.target.localPosition;
                _posRotScaryEvent.startRotation = _posRotScaryEvent.target.localRotation.eulerAngles;
            }


            if (_isEditing && _posRotScaryEvent.target != null)
            {
                Transform targetTransform = _posRotScaryEvent.target.transform;
                if (GUILayout.Button(_finishButtonText))
                {
                    ActiveEditorTracker.sharedTracker.isLocked = false;
                    _isEditing = false;
                    _posRotScaryEvent.finishPosition = targetTransform.localPosition;
                    _posRotScaryEvent.finishRotation = targetTransform.localRotation.eulerAngles;
                }
            }
        }
    }
}