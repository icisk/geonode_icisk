#########################################################################
#
# Copyright (C) 2023 Open Source Geospatial Foundation - all rights reserved
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

import logging
import json
from tastypie.test import TestApiClient
from uuid import uuid4

from django.contrib.auth import get_user_model
from django.http import JsonResponse
from django.test import RequestFactory
from django.urls import reverse

from geonode.base.models import Thesaurus, ThesaurusLabel, ThesaurusKeyword, ThesaurusKeywordLabel, ResourceBase, Region
from geonode.facets.models import facet_registry
from geonode.facets.providers.baseinfo import FeaturedFacetProvider
from geonode.facets.providers.region import RegionFacetProvider
from geonode.tests.base import GeoNodeBaseTestSupport
import geonode.facets.views as views


logger = logging.getLogger(__name__)


class TestFacets(GeoNodeBaseTestSupport):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()

        cls.user = get_user_model().objects.create(username="user_00")
        cls.admin = get_user_model().objects.get(username="admin")

        cls._create_thesauri()
        cls._create_regions()
        cls._create_resources()
        cls.rf = RequestFactory()

    @classmethod
    def tearDownClass(cls):
        super().tearDownClass()
        # remove_models(cls.get_obj_ids, type=cls.get_type, integration=cls.get_integration)

    def setUp(self):
        super().setUp()

        self.api_client = TestApiClient()

        self.assertEqual(self.admin.username, "admin")
        self.assertEqual(self.admin.is_superuser, True)

    @classmethod
    def _create_thesauri(cls):
        cls.thesauri = {}
        cls.thesauri_k = {}

        for tn in range(2):
            t = Thesaurus.objects.create(identifier=f"t_{tn}", title=f"Thesaurus {tn}", order=100 + tn * 10)
            cls.thesauri[tn] = t
            for tl in (
                "en",
                "it",
            ):
                ThesaurusLabel.objects.create(thesaurus=t, lang=tl, label=f"TLabel {tn} {tl}")

            for tkn in range(10):
                tk = ThesaurusKeyword.objects.create(thesaurus=t, alt_label=f"T{tn}_K{tkn}_ALT")
                cls.thesauri_k[f"{tn}_{tkn}"] = tk
                for tkl in (
                    "en",
                    "it",
                ):
                    ThesaurusKeywordLabel.objects.create(keyword=tk, lang=tkl, label=f"T{tn}_K{tkn}_{tkl}")

    @classmethod
    def _create_regions(cls):
        cls.regions = {}

        for code, name in (
            ("R0", "Region0"),
            ("R1", "Region1"),
            ("R2", "Region2"),
        ):
            cls.regions[code] = Region.objects.create(code=code, name=name)

    @classmethod
    def _create_resources(self):
        public_perm_spec = {"users": {"AnonymousUser": ["view_resourcebase"]}, "groups": []}

        for x in range(20):
            d: ResourceBase = ResourceBase.objects.create(
                title=f"dataset_{x:02}",
                uuid=str(uuid4()),
                owner=self.user,
                abstract=f"Abstract for dataset {x:02}",
                subtype="vector",
                is_approved=True,
                is_published=True,
            )

            # These are the assigned keywords to the Resources

            # RB00 ->            T1K0          R0,R1   FEAT
            # RB01 ->  T0K0      T1K0          R0      FEAT
            # RB02 ->            T1K0          R1      FEAT
            # RB03 ->  T0K0      T1K0
            # RB04 ->            T1K0
            # RB05 ->  T0K0      T1K0
            # RB06 ->            T1K0                  FEAT
            # RB07 ->  T0K0      T1K0                  FEAT
            # RB08 ->            T1K0 T1K1     R1      FEAT
            # RB09 ->  T0K0      T1K0 T1K1
            # RB10 ->                 T1K1
            # RB11 ->  T0K0 T0K1      T1K1
            # RB12 ->                 T1K1             FEAT
            # RB13 ->  T0K0 T0K1               R1      FEAT
            # RB14 ->                                  FEAT
            # RB15 ->  T0K0 T0K1
            # RB16 ->
            # RB17 ->  T0K0 T0K1
            # RB18 ->                                  FEAT
            # RB19 ->  T0K0 T0K1                       FEAT

            if x % 2 == 1:
                logger.debug(f"ADDING KEYWORDS {self.thesauri_k['0_0']} to RB {d}")
                d.tkeywords.add(self.thesauri_k["0_0"])
            if x % 2 == 1 and x > 10:
                logger.debug(f"ADDING KEYWORDS {self.thesauri_k['0_1']} to RB {d}")
                d.tkeywords.add(self.thesauri_k["0_1"])
            if x < 10:
                logger.debug(f"ADDING KEYWORDS {self.thesauri_k['1_0']} to RB {d}")
                d.tkeywords.add(self.thesauri_k["1_0"])
            if 7 < x < 13:
                d.tkeywords.add(self.thesauri_k["1_1"])
            if x in (0, 1):
                d.regions.add(self.regions["R0"])
            if x in (0, 2, 8, 13):
                d.regions.add(self.regions["R1"])
            if (x % 6) in (0, 1, 2):
                d.featured = True

            d.save()
            d.set_permissions(public_perm_spec)

    @staticmethod
    def _facets_to_map(facets):
        return {f["name"]: f for f in facets}

    def test_facets_base(self):
        req = self.rf.get(reverse("list_facets"), data={"lang": "en"})
        res: JsonResponse = views.list_facets(req)
        obj = json.loads(res.content)
        self.assertIn("facets", obj)
        facets_list = obj["facets"]
        self.assertEqual(8, len(facets_list))
        fmap = self._facets_to_map(facets_list)
        for name in ("category", "owner", "t_0", "t_1", "featured", "resourcetype", "keyword"):
            self.assertIn(name, fmap)

    def test_facets_rich(self):
        # make sure the resources are in
        c = ResourceBase.objects.count()
        self.assertEqual(20, c)

        # make sure tkeywords have been assigned by checking a sample resource
        rb = ResourceBase.objects.get(title="dataset_01")
        self.assertEqual(2, rb.tkeywords.count())

        # run the request
        req = self.rf.get(reverse("list_facets"), data={"include_topics": 1, "lang": "en"})
        res: JsonResponse = views.list_facets(req)
        obj = json.loads(res.content)

        facets_list = obj["facets"]
        self.assertEqual(8, len(facets_list))
        fmap = self._facets_to_map(facets_list)
        for expected in (
            {
                "name": "category",
                "topics": {
                    "total": 0,
                },
            },
            {
                "name": "owner",
                "topics": {
                    "total": 1,
                },
            },
            {
                "name": "t_0",
                "topics": {
                    "total": 2,
                    "items": [
                        {"label": "T0_K0_en", "count": 10},
                        {"label": "T0_K1_en", "count": 5},
                    ],
                },
            },
            {
                "name": "t_1",
                "topics": {
                    "total": 2,
                    "items": [
                        {"label": "T1_K0_en", "count": 10},
                    ],
                },
            },
            {
                "name": "region",
                "topics": {
                    "total": 2,
                    "items": [
                        {"label": "Region0", "key": "R0", "count": 2},
                        {"label": "Region1", "key": "R1", "count": 4},
                    ],
                },
            },
            {
                "name": "featured",
                "topics": {
                    "total": 2,
                    "items": [
                        {"label": "True", "key": True, "count": 11},
                        {"label": "False", "key": False, "count": 9},
                    ],
                },
            },
            {
                "name": "resourcetype",
                "topics": {
                    "total": 1,
                    "items": [
                        {"label": "resourcebase", "key": "resourcebase", "count": 20},
                    ],
                },
            },
        ):
            name = expected["name"]
            self.assertIn(name, fmap)
            facet = fmap[name]
            expected_topics = expected["topics"]
            for topic_key in expected_topics:
                if topic_key != "items":
                    self.assertEqual(
                        expected_topics[topic_key], facet["topics"][topic_key], f"Mismatching '{topic_key}' for {name}"
                    )
                else:
                    items = facet["topics"]["items"]
                    expected_items = expected_topics["items"]
                    for exp_item in expected_items:
                        exp_label = exp_item["label"]
                        found = None
                        for item in items:
                            if item["label"] == exp_label:
                                found = item
                                break

                        self.assertIsNotNone(
                            found, f"topic not found '{exp_label}' for facet '{name}' -- found items {items}"
                        )
                        for exp_field in exp_item:
                            self.assertEqual(
                                exp_item[exp_field], found[exp_field], f"Mismatch item key:{exp_field} facet:{name}"
                            )

    def test_bad_lang(self):
        # for thesauri, make sure that by requesting a non-existent language the faceting is still working,
        # using the default labels

        # run the request with a valid language
        req = self.rf.get(reverse("get_facet", args=["t_0"]), data={"lang": "en"})
        res: JsonResponse = views.get_facet(req, "t_0")
        obj = json.loads(res.content)

        self.assertEqual(2, obj["topics"]["total"])
        self.assertEqual(10, obj["topics"]["items"][0]["count"])
        self.assertEqual("T0_K0_en", obj["topics"]["items"][0]["label"])
        self.assertTrue(obj["topics"]["items"][0]["is_localized"])

        # run the request with an INVALID language
        req = self.rf.get(reverse("get_facet", args=["t_0"]), data={"lang": "ZZ"})
        res: JsonResponse = views.get_facet(req, "t_0")
        obj = json.loads(res.content)

        self.assertEqual(2, obj["topics"]["total"])
        self.assertEqual(10, obj["topics"]["items"][0]["count"])  # make sure the count is still there
        self.assertEqual("T0_K0_ALT", obj["topics"]["items"][0]["label"])  # check for the alternate label
        self.assertFalse(obj["topics"]["items"][0]["is_localized"])  # check for the localization flag

    def test_topics(self):
        for facet, keys, exp in (
            ("t_0", [self.thesauri_k["0_0"].id, self.thesauri_k["0_1"].id, -999], 2),
            ("category", ["C1", "C2", "nomatch"], 0),
            ("owner", [self.user.id, -100], 1),
            ("region", ["R0", "R1", "nomatch"], 2),
        ):
            req = self.rf.get(reverse("get_facet_topics", args=[facet]), data={"lang": "en", "key": keys})
            res: JsonResponse = views.get_facet_topics(req, facet)
            obj = json.loads(res.content)
            self.assertEqual(exp, len(obj["topics"]["items"]), f"Unexpected topic count {exp} for facet {facet}")

    def test_prefiltering(self):
        reginfo = RegionFacetProvider().get_info()
        t0info = facet_registry.get_provider("t_0").get_info()
        t1info = facet_registry.get_provider("t_1").get_info()

        for facet, filters, totals, count0 in (
            ("t_0", {}, 2, 10),
            ("t_0", {reginfo["key"]: "R0"}, 1, 1),
            ("t_1", {}, 2, 10),
            ("t_1", {reginfo["key"]: "R0"}, 1, 2),
            ("t_1", {reginfo["key"]: "R1"}, 2, 3),
            (reginfo["name"], {}, 2, 4),
            (reginfo["name"], {t0info["key"]: self.thesauri_k["0_0"].id}, 2, 1),
            (reginfo["name"], {t1info["key"]: self.thesauri_k["1_0"].id}, 2, 3),
        ):
            req = self.rf.get(reverse("get_facet", args=[facet]), data=filters)
            res: JsonResponse = views.get_facet(req, facet)
            obj = json.loads(res.content)
            self.assertEqual(totals, obj["topics"]["total"], f"Bad totals for facet '{facet} and filter {filters}")
            self.assertEqual(count0, obj["topics"]["items"][0]["count"], f"Bad count0 for facet '{facet}")

    def test_prefiltering_tkeywords(self):
        regname = RegionFacetProvider().name
        featname = FeaturedFacetProvider().name
        t1filter = facet_registry.get_provider("t_1").get_info()["filter"]
        tkey_1_1 = self.thesauri_k["1_1"].id

        expected_region = {"R1": 1}
        expected_feat = {True: 2, False: 3}

        # Run the single requests
        for facet, params, items in (
            (regname, {t1filter: tkey_1_1}, expected_region),
            (featname, {t1filter: tkey_1_1}, expected_feat),
        ):
            req = self.rf.get(reverse("get_facet", args=[facet]), data=params)
            res: JsonResponse = views.get_facet(req, facet)
            obj = json.loads(res.content)

            self.assertEqual(
                len(items),
                len(obj["topics"]["items"]),
                f"Bad count for items '{facet} \n PARAMS: {params} \n RESULT: {obj} \n EXPECTED: {items}",
            )
            # search item
            for item in items.keys():
                found = next((i for i in obj["topics"]["items"] if i["key"] == item), None)
                self.assertIsNotNone(found, f"Topic '{item}' not found in facet {facet} -- {obj}")
                self.assertEqual(items[item], found.get("count", None), f"Bad count for facet '{facet}:{item}")

        # Run the single request
        req = self.rf.get(reverse("list_facets"), data={"include_topics": 1, t1filter: tkey_1_1})
        res: JsonResponse = views.list_facets(req)
        obj = json.loads(res.content)

        facets_list = obj["facets"]
        fmap = self._facets_to_map(facets_list)

        for name, items in (
            (regname, expected_region),
            (featname, expected_feat),
        ):
            self.assertIn(name, fmap)
            facet = fmap[name]

            for item in items.keys():
                found = next((i for i in facet["topics"]["items"] if i["key"] == item), None)
                self.assertIsNotNone(found, f"Topic '{item}' not found in facet {facet} -- {facet}")
                self.assertEqual(items[item], found.get("count", None), f"Bad count for facet '{facet}:{item}")

    def test_config(self):
        for facet, type, order in (
            ("resourcetype", None, None),
            ("t_0", "select", 100),
            ("category", "select", 5),
            ("region", "select", 7),
            ("owner", "select", 8),
        ):
            req = self.rf.get(reverse("get_facet", args=[facet]), data={"include_config": True})
            res: JsonResponse = views.get_facet(req, facet)
            obj = json.loads(res.content)
            self.assertIn("config", obj, "Config info not found in payload")
            conf = obj["config"]

            if type is None:
                self.assertNotIn("type", conf)
            else:
                self.assertEqual(type, conf["type"], "Unexpected type")

            if order is None:
                self.assertNotIn("order", conf)
            else:
                self.assertEqual(order, conf["order"], "Unexpected order")

    def test_user_auth(self):
        # make sure the user authorization pre-filters the visible resources
        # TODO test
        pass

    def test_thesauri_reloading(self):
        # Thesauri facets are cached.
        # Make sure that when Thesauri or ThesauriLabel change the facets cache is invalidated
        # TODO impl+test
        pass
