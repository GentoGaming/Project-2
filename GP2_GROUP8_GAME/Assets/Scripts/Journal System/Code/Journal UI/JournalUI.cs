using System.Collections.Generic;
using Journal_System.Code.Scripts;
using UnityEngine;
using UnityEngine.UI;

namespace Journal_System.Code.Journal_UI
{
    public class JournalUI : MonoBehaviour
    {
        [SerializeField] private List<Journal> journals;
        public GameObject back;
        public GameObject next;
        public GameObject finish;
        private readonly List<Sprite> _journalSprites = new List<Sprite>();
        private Journal _currentActiveJournal;
        public GameObject _journal;
        private Image _uiJournalImage;

        void Start()
        {
            _journal = transform.Find("Journal").gameObject;
            _uiJournalImage = transform.Find("JournalImage").gameObject.GetComponent<Image>();

            if (journals.Count > 0)
            {
                _currentActiveJournal = journals[0];
                GeneratePics();
            }
            else
            {
                Debug.LogWarning("Journal List is Empty , drag a Journal to GameObject JournalParent inside Canvas");
            }
        }
        public static Sprite ConvertToSprite(Texture2D texture)
        {
            return Sprite.Create(texture, new Rect(0, 0, texture.width, texture.height), Vector2.zero);
        }

        private void GeneratePics()
        {
            if (_currentActiveJournal != null)
            {
                List<JournalNode> nodes = _currentActiveJournal.GetAllNodes();
                foreach (var node in nodes)
                {
                    _journalSprites.Add(ConvertToSprite(node.GetImage()));
                }

                if (_journalSprites.Count > 0)
                {
                    _uiJournalImage.sprite = _journalSprites[0];
                }
            }
        }

        private int GetOurCurrentImageIndex()
        {
            for (int i = 0; i < _journalSprites.Count; i++) // Loop through List with for
            {
                if (_uiJournalImage.sprite == _journalSprites[i]) return i;
            }

            return 0;
        }


        public void NextImage()
        {
            int currentIndex = GetOurCurrentImageIndex();
            if (currentIndex != _journalSprites.Count - 1)
            {
                _uiJournalImage.sprite = _journalSprites[currentIndex + 1];
                ToggleButtons(currentIndex + 1);
            }
        }

        private void ToggleButtons(int currentIndex)
        {
            back.SetActive(currentIndex != 0);
            next.SetActive(currentIndex != _journalSprites.Count - 1);
            finish.SetActive(currentIndex == _journalSprites.Count - 1);
        }

        public void BackImage()
        {
            int currentIndex = GetOurCurrentImageIndex();
            if (currentIndex != 0)
            {
                _uiJournalImage.sprite = _journalSprites[currentIndex - 1];
                ToggleButtons(currentIndex - 1);
            }
        }


        private void ChangeJournal(string journalName)
        {
            foreach (Journal journal in journals)
            {
                if (journal.name == journalName)
                {
                    _currentActiveJournal = journal;
                    GeneratePics();
                    break;
                }
            }
        }

        public void Finish()
        {
            PlayerInput.instance.NullifyInput = false;
            _journal.SetActive(false);
            _uiJournalImage.gameObject.SetActive(false);
            Cursor.lockState = CursorLockMode.Locked;

        }

        public void VisibilityToggle()
        {
            if (Input.GetKeyDown(KeyCode.Tab))
            {
                
                if (_journal.activeSelf)
                {
                    _journal.SetActive(false);
                    _uiJournalImage.gameObject.SetActive(false);
                    Cursor.lockState = CursorLockMode.Locked;
                    PlayerInput.instance.NullifyInput = false;
                }
                else
                {
                    _journal.SetActive(true);
                    _uiJournalImage.gameObject.SetActive(true);
                    Cursor.lockState = CursorLockMode.None;
                    PlayerInput.instance.NullifyInput = true;
                }

                if (_journalSprites.Count > 0)
                {
                    _uiJournalImage.sprite = _journalSprites[0];
                    ToggleButtons(0);
                }
            }
        }
    }
}