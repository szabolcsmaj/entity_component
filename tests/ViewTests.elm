module ViewTests exposing (..)

import Test exposing (..)
import View exposing (..)
import Expect
import String


all : Test
all =
    describe "ViewTests"
        [ test "find closing bracket finds object string" <|
            \() ->
                let
                    sampleText =
                        "this.is.an.object@12345678[some(content{with[multiple[levels]of]different}braces)wow],anotherValue=3,someOtherValue=object.stuff@abcdef12[another[object(here)]]"

                    ( object, _ ) =
                        View.getObjectWithClosingBracket sampleText
                in
                    Expect.equal object "this.is.an.object@12345678[some(content{with[multiple[levels]of]different}braces)wow]"
        , test "find closing bracket finds proper remainder" <|
            \() ->
                let
                    sampleText =
                        "this.is.an.object@12345678[some(content{with[multiple[levels]of]different}braces)wow],anotherValue=3,someOtherValue=object.stuff@abcdef12[another[object(here)]]"

                    ( _, remainder ) =
                        View.getObjectWithClosingBracket sampleText
                in
                    --Notice the missing comma from the beginning!
                    Expect.equal remainder "anotherValue=3,someOtherValue=object.stuff@abcdef12[another[object(here)]]"
        , test "splitValue gives proper result" <|
            \() ->
                let
                    sampleText =
                        "ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@7e6af704,employeeId=1458,loginName=apw,firstName=Andrea,middleName=<null>,lastName=Weisskopf,abacusId=276,joiningDat=2005-09-19 00:00:00.0,resignDat=<null>,poolManager=ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@289b0711[ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@289b0711,employeeId=287,loginName=chris,firstName=Chris,middleName=<null>,lastName=Tanner,abacusId=366,joiningDat=2008-04-01 00:00:00.0,resignDat=<null>,poolManager=ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@289b0711,preferences=ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@4e349407[ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@4e349407,preferencesId=1036,employeeSorting=0,myActivity=<null>,hideAdos=false,employeeLanguage=en,ctlCreUid=patch_2.6.0.0,ctlCreTs=2015-06-06 09:06:57.411187,ctlModUid=patch_2.6.0.0,ctlModTs=2015-06-06 09:06:57.411187,ctlTcn=0],ctlAct=true,ctlCreUid=patch_1.14.0.0,ctlCreTs=2013-10-16 06:18:19.441353,ctlModUid=ira,ctlModTs=2016-03-08 09:23:28.663,ctlTcn=18],preferences=ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@1aea4667[ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@1aea4667,preferencesId=1070,employeeSorting=2,myActivity=<null>,hideAdos=true,employeeLanguage=en,ctlCreUid=patch_2.6.0.0,ctlCreTs=2015-06-06 09:06:57.411187,ctlModUid=apw,ctlModTs=2015-07-17 04:40:27.907,ctlTcn=2],ctlAct=true,ctlCreUid=patch_1.14.0.0,ctlCreTs=2013-10-16 06:18:19.441353,ctlModUid=ira,ctlModTs=2016-03-08 09:23:28.741,ctlTcn=14"

                    expected =
                        [ "ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@7e6af704", "employeeId=1458", "loginName=apw", "firstName=Andrea", "middleName=<null>", "lastName=Weisskopf", "abacusId=276", "joiningDat=2005-09-19 00:00:00.0", "resignDat=<null>", "poolManager=ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@289b0711[ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@289b0711,employeeId=287,loginName=chris,firstName=Chris,middleName=<null>,lastName=Tanner,abacusId=366,joiningDat=2008-04-01 00:00:00.0,resignDat=<null>,poolManager=ch.adnovum.itc.datamodel.entities.common.EmployeeEntity@289b0711,preferences=ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@4e349407[ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@4e349407,preferencesId=1036,employeeSorting=0,myActivity=<null>,hideAdos=false,employeeLanguage=en,ctlCreUid=patch_2.6.0.0,ctlCreTs=2015-06-06 09:06:57.411187,ctlModUid=patch_2.6.0.0,ctlModTs=2015-06-06 09:06:57.411187,ctlTcn=0],ctlAct=true,ctlCreUid=patch_1.14.0.0,ctlCreTs=2013-10-16 06:18:19.441353,ctlModUid=ira,ctlModTs=2016-03-08 09:23:28.663,ctlTcn=18]", "preferences=ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@1aea4667[ch.adnovum.itc.datamodel.entities.common.PreferencesEntity@1aea4667,preferencesId=1070,employeeSorting=2,myActivity=<null>,hideAdos=true,employeeLanguage=en,ctlCreUid=patch_2.6.0.0,ctlCreTs=2015-06-06 09:06:57.411187,ctlModUid=apw,ctlModTs=2015-07-17 04:40:27.907,ctlTcn=2]", "ctlAct=true", "ctlCreUid=patch_1.14.0.0", "ctlCreTs=2013-10-16 06:18:19.441353", "ctlModUid=ira", "ctlModTs=2016-03-08 09:23:28.741", "ctlTcn=14" ]
                in
                    Expect.equal expected (View.splitValue sampleText)
        ]
