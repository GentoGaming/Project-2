using System.Collections.Generic;
using System.Linq;
using Journal_System.Code.Scripts;
using UnityEditor;
using UnityEditor.Callbacks;
using UnityEngine;

namespace Journal_System.Code.Editor
{
    public class JournalEditor : EditorWindow
    {
        private const int Padding = 15;

        private static readonly Color InsideColor = new Color(0f, 0.05f, 0.21f, 0.5f),
            SeparatorColor = new Color(0.96f, 0f, 0.14f, 0.87f),
            ConnectionColor = new Color(0.99f, 1f, 0.98f);

        private static string _finalPath = "";
        static Journal _mainJournal;

        private static DefaultAsset _targetFolder;
        private static bool _forceRefresh = true;
        private static string _tempPath = "";


        private readonly Rect _journalOuterRect = new Rect(400, 10, 400, 150),
            _journalHeaderRect = new Rect(0, 0, 400, 30),
            _nodeOuterRect = new Rect(20, 300, 250, 300);

        readonly Vector2 _scrollViewSize = new Vector2(5000, 5000);

        private GUIStyle _innerStyle, _outerStyle, _headerStyle, _orderStyle;

        private List<Texture2D> _picsInFolder = new List<Texture2D>();
        Vector2 _scrollViewPosition, _scrollViewOffsetDrag;
        private List<Texture2D> _tempFolder = new List<Texture2D>();

        private void OnEnable()
        {
            InitRectStyles();
            Selection.selectionChanged += DialogueSelectionIsChanged;
        }


        private void OnGUI()
        {
            RefreshIt();
            if (_forceRefresh)
            {
                RefreshIt();
                _forceRefresh = false; // refreshing one time on asset open.
            }

            MouseKeyboardEvents();


            if (_mainJournal == null)
            {
                EditorGUILayout.LabelField("Please Select A Dialogue.");
                DialogueSelectionIsChanged();
            }
            else
            {
                EditorGUI.BeginChangeCheck();
                _scrollViewPosition = EditorGUILayout.BeginScrollView(_scrollViewPosition);
                EditorGUI.EndChangeCheck();
                Rect canvas = GUILayoutUtility.GetRect(_scrollViewSize.x, _scrollViewSize.y);
                DrawChooseFolder();

                if (_mainJournal == null || _finalPath == null || _mainJournal.GetAllNodes().Count == 0 ||
                    _picsInFolder == null || _picsInFolder.Count == 0)
                {
                    EditorGUILayout.EndScrollView();
                    return;
                }

                foreach (JournalNode node in _mainJournal.GetAllNodes())
                {
                    DrawLinesAsConnections(node);
                }


                foreach (JournalNode node in _mainJournal.GetAllNodes())
                {
                    DrawNode(node);
                }

                EditorGUILayout.EndScrollView();
            }
        }

        private void RefreshIt()
        {
            GetImages();

            if (_mainJournal != null && _mainJournal.IsFolderPathNotEmpty())
            {
                SaveDeleteAndCreateNodes();
            }
        }

        private void SaveDeleteAndCreateNodes()
        {
            GetImages();

            foreach (JournalNode nodeT in _mainJournal.GetAllNodes().ToList())
            {
                bool isExist = false;
                foreach (Texture2D texture in _picsInFolder)
                {
                    if (nodeT.GetImageName().Equals(texture.name))
                    {
                        isExist = true;
                        break;
                    }
                }

                if (!isExist)
                {
                    _mainJournal.DeletingJournal(nodeT);
                    _forceRefresh = true;
                }
            }

            _mainJournal.OrderList(new Vector2(_nodeOuterRect.width, _nodeOuterRect.height));

            foreach (Texture2D texture in _picsInFolder)
            {
                bool shouldSkip = false;
                foreach (JournalNode node in _mainJournal.GetAllNodes())
                {
                    if (node.GetImageName().Equals(texture.name))
                    {
                        shouldSkip = true;
                        break;
                    }
                }

                if (shouldSkip) continue;

                _mainJournal.CreatingJournal(texture);
                _forceRefresh = true;
            }
        }

        private void LineConnectionPosition(Vector3 startPos, Vector3 endPos)
        {
            Vector3 controlPointOffset = endPos - startPos;
            controlPointOffset.y = 0;
            controlPointOffset.x *= 0.8f;
            Handles.DrawBezier(
                startPos, endPos,
                startPos + controlPointOffset,
                endPos - controlPointOffset,
                ConnectionColor, null, 10f);
        }

        private void DrawLinesAsConnections(JournalNode nodeTemp)
        {
            JournalNode nextNode = null;
            foreach (JournalNode node in _mainJournal.GetAllNodes())
            {
                if (node.GetOrder() == (nodeTemp.GetOrder() + 1))
                {
                    nextNode = node;
                }
            }

            if (nextNode != null)
            {
                LineConnectionPosition(new Vector2(nodeTemp.GetRect().xMax, nodeTemp.GetRect().center.y),
                    new Vector2(nextNode.GetRect().xMin, nextNode.GetRect().center.y));
            }
        }


        private JournalNode GetMouseOnDialogue()
        {
            JournalNode foundNode = null;
            Vector2 temp = _scrollViewPosition;

            // if(!isOffset) temp = Vector2.zero;
            if (_mainJournal != null)
            {
                foreach (JournalNode node in _mainJournal.GetAllNodes())
                {
                    if (node.GetRect().Contains(Event.current.mousePosition + temp))
                    {
                        foundNode = node;
                    }
                }
            }

            return foundNode;
        }

        private void MouseKeyboardEvents()
        {
            if (Event.current.type == EventType.MouseDown && Event.current.type != EventType.MouseDrag)
            {
                _scrollViewOffsetDrag = Event.current.mousePosition + _scrollViewPosition;
                JournalNode jNode = GetMouseOnDialogue();
                if (jNode != null)
                {
                    Selection.activeObject = jNode;
                }
            }
            else if (Event.current.type == EventType.MouseDrag)
            {
                _scrollViewPosition = _scrollViewOffsetDrag - Event.current.mousePosition;
                GUI.changed = true;
            }
        }

        private void DrawNode(JournalNode node)
        {
            GUILayout.BeginArea(node.GetRect());
            Texture2D texture = null;
            foreach (Texture2D t in _picsInFolder)
            {
                if (node.GetImageName().Equals(t.name))
                {
                    texture = t;
                }
            }

            GUI.DrawTexture(new Rect(0, 0, _nodeOuterRect.width, _nodeOuterRect.height), texture,
                ScaleMode.StretchToFill, true, 0f);
            GUILayout.Space(10);
            GUILayout.BeginHorizontal();

            int order = node.GetOrder();
            if (order > 1)
            {
                if (GUILayout.Button(" < ", GUILayout.Width(15), GUILayout.Height(15)))
                {
                    NodeOrder(node, order, false);
                }
            }
            else
            {
                GUILayout.Label("", GUILayout.Width(0));
            }

            if (order < _mainJournal.GetAllNodes().Count)
            {
                if (GUILayout.Button(" > ", GUILayout.Width(15), GUILayout.Height(15)))
                {
                    NodeOrder(node, order, true);
                }
            }
            else
            {
                GUILayout.Label("", GUILayout.Width(0));
            }


            GUILayout.EndHorizontal();
            GUILayout.Space(220);
            GUILayout.Label("Order: " + node.GetOrder(), _orderStyle, GUILayout.Width(70));

            GUILayout.EndArea();
        }

        private void SelectANewJournal(Journal journal)
        {
            _mainJournal = journal;

            if (_finalPath == "")
            {
                _finalPath = AssetDatabase.GetAssetPath(_mainJournal.GetFolderPath());
                if (_finalPath != null && !_finalPath.Equals(""))
                {
                    _targetFolder = _mainJournal.GetFolderPath();
                }
            }

            Repaint();
            RefreshIt();
            _forceRefresh = true;
        }

        private void NodeOrder(JournalNode node, int order, bool isForwardSwap)
        {
            JournalNode nextNode = null;
            int newOrder = 1;
            foreach (JournalNode nodeT in _mainJournal.GetAllNodes())
            {
                newOrder = isForwardSwap ? order + 1 : order - 1;

                if (nodeT.GetOrder() == newOrder)
                {
                    nextNode = nodeT;
                }
            }

            node.SetOrder(newOrder);

            if (nextNode != null)
            {
                nextNode.SetOrder(order);
            }
        }


        private void DrawChooseFolder()
        {
            GUILayout.BeginArea(_journalOuterRect, _innerStyle);

            // Header - Has a delete Button
            GUILayout.BeginArea(_journalHeaderRect, _outerStyle);
            GUILayout.Label(_mainJournal.name, _headerStyle);
            GUILayout.EndArea();
            // End Header
            for (int i = 0; i < 5; i++)
            {
                EditorGUILayout.Space();
            }

            EditorGUI.BeginChangeCheck();
            _targetFolder = (DefaultAsset) EditorGUILayout.ObjectField(
                "Select Folder",
                _targetFolder,
                typeof(DefaultAsset),
                true);
            EditorGUI.EndChangeCheck();
            if (_targetFolder != null)
            {
                _tempPath = AssetDatabase.GetAssetPath(_targetFolder);
                EditorGUILayout.HelpBox(
                    "Valid folder! Name: " + _finalPath,
                    MessageType.None,
                    true);

                EditorGUILayout.Space();
                EditorGUILayout.Space();
                GUILayout.BeginHorizontal();
                if (GUILayout.Button("Confirm Path", GUILayout.Width(100)))
                {
                    _finalPath = _tempPath;
                    _mainJournal.SetFolderPath(_targetFolder);
                    GetImages();
                }

                _tempFolder = FindAssetsByType<Texture2D>(_tempPath);

                GUILayout.Label("Found " + _tempFolder.Count + " Images");
                GUILayout.EndHorizontal();
            }
            else
            {
                EditorGUILayout.HelpBox(
                    "Not valid!",
                    MessageType.Warning,
                    true);
            }

            GUILayout.EndArea();
        }


        private List<T> FindAssetsByType<T>(string path) where T : Object
        {
            string[] guids = AssetDatabase.FindAssets("t:texture2D", new[] {path});

            return guids.Select(AssetDatabase.GUIDToAssetPath).Select(AssetDatabase.LoadAssetAtPath<T>)
                .Where(asset => asset != null).ToList();
        }


        private void GetImages()
        {
            if (_finalPath == null || _finalPath.Equals("")) return;
            _picsInFolder = FindAssetsByType<Texture2D>(_finalPath);
        }


        private static Texture2D CreateTexture(Color color)
        {
            var texture = new Texture2D(1, 1);
            var fillColorArray = texture.GetPixels();

            for (var i = 0; i < fillColorArray.Length; ++i)
            {
                fillColorArray[i] = color;
            }

            texture.SetPixels(fillColorArray);

            texture.Apply();
            return texture;
        }

        private static void DisplayEditorWindow()
        {
            GetWindow(typeof(JournalEditor), false, "Journal Editor");
        }


        private void InitRectStyles()
        {
            _headerStyle = new GUIStyle
            {
                fontSize = 15,
                normal = {textColor = Color.white},
                padding = new RectOffset(5, 0, 5, 0),
            };

            _outerStyle = new GUIStyle
            {
                normal = {background = CreateTexture(SeparatorColor)},
                padding = new RectOffset(0, 0, 0, 0),
            };

            _innerStyle = new GUIStyle
            {
                normal = {background = CreateTexture(InsideColor)},
                padding = new RectOffset(Padding, Padding, Padding, Padding),
            };

            _orderStyle = new GUIStyle
            {
                padding = new RectOffset(80, Padding, Padding, Padding),
            };
        }

        private static void ResetValues()
        {
            _mainJournal = null;
            _finalPath = "";
            _targetFolder = null;
            _tempPath = "";
        }

        private void DialogueSelectionIsChanged()
        {
            Journal journal = Selection.activeObject as Journal;
            if (journal == null) return;
            ResetValues();
            SelectANewJournal(journal);
        }


        [OnOpenAsset(1)]
        public static bool OnOpenAsset(int id, int line)
        {
            ResetValues();

            Journal journal = EditorUtility.InstanceIDToObject(id) as Journal;
            if (journal != null)
            {
                _mainJournal = journal;

                if (_finalPath == "")
                {
                    _finalPath = AssetDatabase.GetAssetPath(_mainJournal.GetFolderPath());
                    if (_finalPath != null && !_finalPath.Equals(""))
                    {
                        _targetFolder = _mainJournal.GetFolderPath();
                    }
                }

                DisplayEditorWindow();
                _forceRefresh = true;
                return true;
            }

            return false;
        }
    }
}