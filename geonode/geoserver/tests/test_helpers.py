#########################################################################
#
# Copyright (C) 2019 OSGeo
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#########################################################################
import re
import os
import time
import logging

from urllib.parse import urljoin
from pathvalidate import ValidationError

from django.conf import settings
from django.urls import reverse

from geonode import geoserver, GeoNodeException
from geonode.utils import safe_path_leaf
from geonode.decorators import on_ogc_backend
from geonode.base.populate_test_data import all_public, create_models, remove_models
from geonode.layers.populate_datasets_data import create_dataset_data
from geonode.tests.base import GeoNodeBaseTestSupport
from geonode.geoserver.views import _response_callback
from geonode.geoserver.helpers import (
    gs_catalog,
    ows_endpoint_in_path,
    get_dataset_storetype,
    extract_name_from_sld,
    get_dataset_capabilities_url,
)
from geonode.geoserver.ows import _wcs_link, _wfs_link, _wms_link


logger = logging.getLogger(__name__)


class HelperTest(GeoNodeBaseTestSupport):
    type = "dataset"

    fixtures = ["initial_data.json", "group_test_data.json", "default_oauth_apps.json"]

    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        create_models(type=cls.get_type, integration=cls.get_integration)
        all_public()

    @classmethod
    def tearDownClass(cls):
        super().tearDownClass()
        remove_models(cls.get_obj_ids, type=cls.get_type, integration=cls.get_integration)

    def setUp(self):
        super().setUp()
        self.user = "admin"
        self.passwd = "admin"
        create_dataset_data()

    @on_ogc_backend(geoserver.BACKEND_PACKAGE)
    def test_extract_name_from_sld(self):
        content = """<?xml version="1.0" standalone="yes"?>
<!DOCTYPE foo [ <!ENTITY ent SYSTEM "/etc/passwd" > ]>
<foo xmlns="http://www.opengis.net/sld">
<NamedLayer>
    <UserStyle>
        <Name>&ent;</Name>
    </UserStyle>
</NamedLayer>
</foo>"""
        self.assertIsNone(extract_name_from_sld(gs_catalog, content))

    @on_ogc_backend(geoserver.BACKEND_PACKAGE)
    def test_safe_path_leaf(self):
        base_path = settings.MEDIA_ROOT

        malformed_paths = ["c:/etc/passwd", "c:\\etc\\passwd", "\0_a*b:c<d>e%f/(g)h+i_0.txt"]
        for _path in malformed_paths:
            with self.assertRaises(ValidationError):
                safe_path_leaf(_path)

        unsafe_paths = [
            "/root/",
            "~/.ssh",
            "$HOME/.ssh",
            "/etc/passwd",
            ".../style.sld",
            'fi:l*e/p"a?t>h|.t<xt',
            os.path.join("/tmp/uploaded/", "style.sld"),
            os.path.join(base_path, "../etc/passwd", "style.sld"),
        ]
        for _path in unsafe_paths:
            with self.assertRaisesMessage(
                GeoNodeException,
                f"The provided path '{_path}' is not safe. The file is outside the MEDIA_ROOT '{base_path}' base path!",
            ):
                safe_path_leaf(_path)

        safe_path = os.path.join(base_path, "style.sld")
        self.assertEqual(safe_path_leaf(safe_path), safe_path)

    @on_ogc_backend(geoserver.BACKEND_PACKAGE)
    def test_replace_callback(self):
        content = f"""<Layer>
      <Title>GeoNode Local GeoServer</Title>
      <Abstract>This is a description of your Web Map Server.</Abstract>
      <!--Limited list of EPSG projections:-->
      <CRS>EPSG:4326</CRS>
      <CRS>EPSG:3785</CRS>
      <CRS>EPSG:3857</CRS>
      <CRS>EPSG:900913</CRS>
      <CRS>EPSG:32647</CRS>
      <CRS>EPSG:32736</CRS>
      <CRS>CRS:84</CRS>
      <EX_GeographicBoundingBox>
        <westBoundLongitude>-124.731422</westBoundLongitude>
        <eastBoundLongitude>12.512771464573753</eastBoundLongitude>
        <southBoundLatitude>12.4801497</southBoundLatitude>
        <northBoundLatitude>49.371735</northBoundLatitude>
      </EX_GeographicBoundingBox>
      <BoundingBox CRS="CRS:84" ..../>
      <BoundingBox CRS="EPSG:4326" ..../>
      <BoundingBox CRS="EPSG:3785" ..../>
      <BoundingBox CRS="EPSG:3857" ..../>
      <BoundingBox CRS="EPSG:900913" ..../>
      <BoundingBox CRS="EPSG:32647" ..../>
      <BoundingBox CRS="EPSG:32736" ..../>
      <Layer queryable="1" opaque="0">
        <Name>geonode:DE_USNG_UTM18</Name>
        <Title>DE_USNG_UTM18</Title>
        <Abstract>No abstract provided</Abstract>
        <KeywordList>
          <Keyword>DE_USNG_UTM18</Keyword>
          <Keyword>features</Keyword>
        </KeywordList>
        <CRS>EPSG:26918</CRS>
        <CRS>CRS:84</CRS>
        <EX_GeographicBoundingBox>
          <westBoundLongitude>-75.93570725669369</westBoundLongitude>
          <eastBoundLongitude>-75.00000000000001</eastBoundLongitude>
          <southBoundLatitude>38.3856300861002</southBoundLatitude>
          <northBoundLatitude>39.89406880610797</northBoundLatitude>
        </EX_GeographicBoundingBox>
        <BoundingBox CRS="CRS:84" .01" maxy="39.89406880610797"/>
        <BoundingBox CRS="EPSG:26918" ..../>
        <BoundingBox CRS="EPSG:4326" ..../>
        <BoundingBox CRS="EPSG:3785" ..../>
        <BoundingBox CRS="EPSG:3857" ..../>
        <BoundingBox CRS="EPSG:900913" ..../>
        <BoundingBox CRS="EPSG:32647" ..../>
        <BoundingBox CRS="EPSG:32736" ..../>
        <MetadataURL type="other">
          <Format>other</Format>
          <OnlineResource xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}catalogue/csw?outputschema=...."/>
        </MetadataURL>
        <MetadataURL type="other">
          <Format>other</Format>
          <OnlineResource xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}catalogue/csw?outputschema=...."/>
        </MetadataURL>
        <MetadataURL type="other">
          <Format>other</Format>
          <OnlineResource xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}catalogue/csw?outputschema=...."/>
        </MetadataURL>
        <MetadataURL type="other">
          <Format>other</Format>
          <OnlineResource xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}catalogue/csw?outputschema=...."/>
        </MetadataURL>
        <MetadataURL type="FGDC">
          <Format>text/xml</Format>
          <OnlineResource xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}catalogue/csw?outputschema=...."/>
        </MetadataURL>
        <MetadataURL type="other">
          <Format>other</Format>
          <OnlineResource xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}catalogue/csw?outputschema=...."/>
        </MetadataURL>
        <MetadataURL type="other">
          <Format>other</Format>
          <OnlineResource xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}showmetadata/xsl/584"/>
        </MetadataURL>
        <Style>
          <Name>geonode:DE_USNG_UTM18</Name>
          <Title>Default Polygon</Title>
          <Abstract>A sample style that draws a polygon</Abstract>
          <LegendURL width="20" height="20">
            <Format>image/png</Format>
            <OnlineResource
xmlns:xlink="http://www.w3.org/1999/xlink" xlink:type="simple"
xlink:href="{settings.GEOSERVER_LOCATION}ows?service=WMS&amp;request=GetLegendGraphic&...."/>
          </LegendURL>
        </Style>
      </Layer>"""
        kwargs = {"content": content, "status": 200, "content_type": "application/xml"}
        _content = _response_callback(**kwargs).content
        self.assertTrue(re.findall(f'{urljoin(settings.SITEURL, "/gs/")}ows', str(_content)))

        kwargs = {"content": content, "status": 200, "content_type": "text/xml; charset=UTF-8"}
        _content = _response_callback(**kwargs).content
        self.assertTrue(re.findall(f'{urljoin(settings.SITEURL, "/gs/")}ows', str(_content)))

    def test_return_element_if_not_exists_in_the_subtypes(self):
        el = get_dataset_storetype("not-existing-type")
        self.assertEqual("not-existing-type", el)

    def test_datastore_should_return_vector(self):
        el = get_dataset_storetype("dataStore")
        self.assertEqual("vector", el)

    def test_coverageStore_should_return_raster(self):
        el = get_dataset_storetype("coverageStore")
        self.assertEqual("raster", el)

    def test_remoteStore_should_return_remote(self):
        el = get_dataset_storetype("remoteStore")
        self.assertEqual("remote", el)

    @on_ogc_backend(geoserver.BACKEND_PACKAGE)
    def test_geoserver_proxy_strip_paths(self):
        response = self.client.get(
            f"{reverse('ows_endpoint')}?service=WFS&version=1.1.0&request=DescribeFeatureType&typeName=geonode:tipi_forestali&outputFormat=application/json&access_token=something"
        )
        self.assertEqual(response.status_code, 200)

    @on_ogc_backend(geoserver.BACKEND_PACKAGE)
    def test_ows_links(self):
        ows_url = "http://foo.org/ows"
        identifier = "foo:fake_alternate"
        min_x, min_y, max_x, max_y = -1, -1, 1, 1
        expected_url = f"{ows_url}?service=WCS&request=GetCoverage&coverageid=foo__fake_alternate&format=image%2Ftiff&version=2.0.1&compression=DEFLATE&tileWidth=512&tileHeight=512&outputCrs=4326"
        download_url = _wcs_link(
            ows_url,
            identifier,
            "image/tiff",
            srid="4326",
            bbox=[min_x, min_y, max_x, max_y],
            compression="DEFLATE",
            tile_size=512,
        )
        self.assertEqual(download_url, expected_url, download_url)

        expected_url = f"{ows_url}?service=WFS&version=1.0.0&request=GetFeature&typename=foo%3Afake_alternate&outputFormat=application%2Fzip&srs=4326&bbox=%5B-1%2C+-1%2C+1%2C+1%5D"
        download_url = _wfs_link(
            ows_url, identifier, "application/zip", {}, srid="4326", bbox=[min_x, min_y, max_x, max_y]
        )
        self.assertEqual(download_url, expected_url, download_url)

        expected_url = f"{ows_url}?service=WMS&request=GetMap&layers=foo%3Afake_alternate&format=image%2Fpng&height=512&width=512&srs=4326&bbox=%5B-1%2C+-1%2C+1%2C+1%5D"
        download_url = _wms_link(
            ows_url, identifier, "image/png", 512, 512, srid="4326", bbox=[min_x, min_y, max_x, max_y]
        )
        self.assertEqual(download_url, expected_url, download_url)

    @on_ogc_backend(geoserver.BACKEND_PACKAGE)
    def test_ows_endpoint_in_path(self):
        path = "http://localhost:8080ows/rest/rest/"

        self.assertIsNotNone(ows_endpoint_in_path(path))
        self.assertEqual(len(ows_endpoint_in_path(path).groups()), 1)

        start_time = time.time()
        ows_endpoint_in_path(path)
        end_time_1 = time.time() - start_time

        start_time = time.time()
        re.match(r".*/(rest)/.*$", path, re.IGNORECASE)
        end_time_2 = time.time() - start_time

        self.assertLess(end_time_1, end_time_2)

    @on_ogc_backend(geoserver.BACKEND_PACKAGE)
    def test_dataset_capabilties_url(self):
        from geonode.layers.models import Dataset

        ows_url = settings.GEOSERVER_PUBLIC_LOCATION
        identifier = "geonode:CA"
        dataset = Dataset.objects.get(alternate=identifier)
        expected_url = f"{ows_url}geonode/CA/ows?service=wms&version=1.3.0&request=GetCapabilities"
        capabilities_url = get_dataset_capabilities_url(dataset)
        self.assertEqual(capabilities_url, expected_url, capabilities_url)
