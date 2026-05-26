import json
from pathlib import Path, PurePath


def test_submission_config_is_repo_relative():
    config_path = Path(__file__).parent.parent / "e156-submission" / "config.json"
    config = json.loads(config_path.read_text(encoding="utf-8"))

    assert config["slug"] == "kmextract"
    assert config["title"].startswith("KMextract:")
    assert not PurePath(config["path"]).is_absolute()
    assert config["path"] == ".."


def test_release_surface_has_no_hardcoded_local_paths():
    repo_root = Path(__file__).parent.parent
    checked = [
        repo_root / "README.md",
        repo_root / "E156-PROTOCOL.md",
        repo_root / "e156-submission" / "config.json",
        repo_root / "km_pdf_vector_extract_ultra.R",
    ]
    forbidden = (
        "C:\\Users\\user",
        "C:\\KMextract",
        "OneDrive - NHS",
        "file:///C:/",
    )

    for path in checked:
        text = path.read_text(encoding="utf-8")
        for marker in forbidden:
            assert marker not in text, f"{path.name} leaked {marker}"


def test_readme_matches_shipped_extractor_surface():
    repo_root = Path(__file__).parent.parent
    readme = (repo_root / "README.md").read_text(encoding="utf-8")

    assert "km_pdf_vector_extract_ultra.R" in readme
    assert "python -m pytest -q" in readme
    assert (repo_root / "km_pdf_vector_extract_ultra.R").exists()
