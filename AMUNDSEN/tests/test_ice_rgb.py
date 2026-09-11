from PIL import Image
from dashboard import ice_store


def test_slice_mean_is_cached_and_returned_in_track(tmp_path):
    db=ice_store.connect(tmp_path)
    identifier='a'*20
    db.execute("INSERT INTO photos(id,file,t,leg) VALUES (?,?,?,?)",(identifier,'photo.jpg',1,'leg'))
    (tmp_path/'images').mkdir()
    image=Image.new('RGB',(4,180),(20,40,60))
    image.save(ice_store.photo_path(identifier,'slice',tmp_path),format='PNG')
    assert ice_store.cache_slice_rgb(db,identifier,tmp_path)==[20,40,60]
    db.commit();db.close()
    assert ice_store.track(0,2,tmp_path)['photos'][0]['rgb']==[20,40,60]


def test_old_cache_without_rgb_still_serves_track(tmp_path):
    db=ice_store.connect(tmp_path)
    db.execute('DROP TABLE photo_rgb')
    db.execute("INSERT INTO photos(id,file,t,leg) VALUES ('a','file',1,'leg')")
    db.commit();db.close()
    assert ice_store.track(0,2,tmp_path)['photos'][0]['rgb'] is None
