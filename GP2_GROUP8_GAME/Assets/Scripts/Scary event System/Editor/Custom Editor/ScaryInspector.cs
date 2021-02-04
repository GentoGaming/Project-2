using System.Collections.Generic;
using Scary_event_System.Gizmo_And_Addons;
using Scary_event_System.ScaryEventUI;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Editor.Custom_Editor

{
    [CustomEditor(typeof(GenericScaryEvent))]
    public class ScaryInspector : UnityEditor.Editor
    {
        public override void OnInspectorGUI()
        {
            GenericScaryEvent gen = this.target as GenericScaryEvent;
            if (gen != null)
            {
                EditorGUILayout.BeginHorizontal();
                GUILayout.Label("Event ID: ");
                GUILayout.Label(gen.GetEventID());
                if (GUILayout.Button("Copy To Clipboard"))
                {
                    GUIUtility.systemCopyBuffer = "EventManager.TriggerEvent (\"" + gen.GetEventID() + "\");";
                }

                EditorGUILayout.EndHorizontal();

                if (GUILayout.Button("Add a Blood Trail scary event"))
                {
                    gen.AddBloodTrailEventScript();
                }

                if (GUILayout.Button("Add Object Shake scary event"))
                {
                    gen.AddObjectShakeEventScript();
                }

                if (GUILayout.Button("Add a Foot Trail scary event"))
                {
                    gen.AddFootStepEventScript();
                }

                if (GUILayout.Button("Add a Change Pos And Rotation scary event"))
                {
                    gen.AddPosRotEventScript();
                }

                if (GUILayout.Button("Add a Sound scary event"))
                {
                    gen.AddSoundEventScript();
                }

                if (GUILayout.Button("Add a Particle scary event"))
                {
                    gen.AddParticleEventScript();
                }


                if (GUILayout.Button("Add a FMOD Audio event"))
                {
                    gen.AddFmodScaryEventScript();
                }

                if (GUILayout.Button("Play Events"))
                {
                    List<ScaryEventAbstract> scaryEventsList = new List<ScaryEventAbstract>();
                    MonoBehaviour[] scripts = Selection.activeGameObject.GetComponentsInChildren<MonoBehaviour>();
                    foreach (var script in scripts)
                    {
                        var thisScript = script as ScaryEventAbstract;
                        if (thisScript == null) continue;
                        scaryEventsList.Add(thisScript);
                    }

                    foreach (var script in scaryEventsList)
                    {
                        if (script.GetType() == typeof(ObjectsShakeScaryEvent))
                        {
                            script.ResetFirstUse();
                        }
                        script.ConditionExecute();
                    }
                }
            }
        }
    }
}