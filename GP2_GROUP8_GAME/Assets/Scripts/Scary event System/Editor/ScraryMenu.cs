using System;
using Scary_event_System.Gizmo_And_Addons;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Editor
{
    public class MenuActionClass
    {
        public enum CallbackAction
        {
            CreateTriggerBox,
            CreateGlobalNoise
        };

        public Enum action;
        public Event currentEvent;
        public Vector3 mousePosition;
        public SceneView sceneView;
    }

    [InitializeOnLoad]
    public class ScaryMenu : EditorWindow
    {
        public static GameObject allScaryEventsGameObj;
        public static bool firstCheck = true;

        static ScaryMenu()
        {
            CreateRootObj();
            SceneView.duringSceneGui += OnSceneGUI;
        }

        private static void CreateRootObj()
        {
            firstCheck = false;
            allScaryEventsGameObj = GameObject.Find("Scary Events");
            if (allScaryEventsGameObj) return;
            GameObject obj = new GameObject("Scary Events");
            allScaryEventsGameObj = obj;
            allScaryEventsGameObj.AddComponent<GizmoScript>();
        }


        private static void OnSceneGUI(SceneView scView)
        {
            Event currentEvent = Event.current;
            bool isKeyDown = currentEvent.type == EventType.KeyDown && currentEvent.keyCode == KeyCode.F1;

            if (isKeyDown)
            {
                GenericMenu menu = new GenericMenu();
                MenuActionClass info = new MenuActionClass
                {
                    mousePosition = currentEvent.mousePosition,
                    action = MenuActionClass.CallbackAction.CreateTriggerBox,
                    sceneView = scView,
                    currentEvent = currentEvent
                };

                menu.AddItem(new GUIContent("Generic area trigger"), false, Callback, info);
                menu.ShowAsContext();
            }
        }

        private static void DrawEventsLabels()
        {
            if (!allScaryEventsGameObj) return;

            foreach (Transform child in allScaryEventsGameObj.transform)
            {
                Handles.Label(child.transform.position, "Area Trigger");
            }
        }

        private static Vector3 GetPositionAtMouseLocation(SceneView scene, Vector3 mousePosition)
        {
            Vector3 mousePos = mousePosition;
            mousePos.y = scene.camera.pixelHeight - mousePos.y * EditorGUIUtility.pixelsPerPoint;
            mousePos.x *= EditorGUIUtility.pixelsPerPoint;
            return mousePos;
        }


        static void Callback(object obj)
        {
            if (!(obj is MenuActionClass callBackInformation)) return;
            MenuActionClass menu = (MenuActionClass) obj;
            Ray ray = menu.sceneView.camera.ScreenPointToRay(GetPositionAtMouseLocation(menu.sceneView,
                menu.mousePosition));
            RaycastHit hit;

            if (Physics.Raycast(ray, out hit))
            {
                CreateRootObj();
                GameObject go = GameObject.CreatePrimitive(PrimitiveType.Cube);
                Vector3 position1;
                position1 = hit.point;
                position1 = new Vector3(position1.x,1f,position1.z );

                go.transform.position = position1;
                Collider collider = go.GetComponent<Collider>();
                //MeshRenderer meshRenderer = go.GetComponent<MeshRenderer>();
                go.AddComponent<GenericScaryEvent>();

                collider.isTrigger = true;
                Tools.current = Tool.Move;
                Selection.activeGameObject = go;
                go.transform.parent = allScaryEventsGameObj.transform;
            }

            menu.currentEvent.Use();
        }
    }
}