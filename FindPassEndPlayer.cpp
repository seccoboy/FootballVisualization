// TRICKY: (31-Jul-2019) ACTIONS DO NOT CONTAIN AN "END PLAYER" ATTRIBUTE, SO IT SHOULD BE INFERRED FROM ACTIONS THAT FOLLOW THE PASS!
int CWhoScoredApp::FindPassEndPlayer(std::vector<ACTION>::const_iterator actionIterator, const std::vector<ACTION>& actionArray) const
{
    HEALTH_CHECK(actionIterator == actionArray.end(), my::Null<int>());
    HEALTH_CHECK(actionArray.empty(), my::Null<int>());

    // TESTING: (27-Apr-2018) UNSUCCESSFUL PASSES, NO END PLAYER
    if ((actionIterator->m_soccerEventType == my::sport::PASS_SOCCER_EVENT) &&
        !actionIterator->m_isSuccessful)
    {
        return my::Null<int>();
    }

    // TESTING: (27-Apr-2018) EVENTS THAT ARE ALSO TAKEN AS PASSES, NO END PLAYER
    if ((actionIterator->m_soccerEventType == my::sport::GOAL_SOCCER_EVENT) ||
        (actionIterator->m_soccerEventType == my::sport::MISSED_SHOTS_SOCCER_EVENT) ||
        (actionIterator->m_soccerEventType == my::sport::SAVED_SHOT_SOCCER_EVENT) ||
		(actionIterator->m_soccerEventType == my::sport::SHOT_ON_POST_SOCCER_EVENT) ||
		// TRICKY: (27-Apr-2018) QUALIFIERS THAT ARE TAKEN AS EVENTS ("Cross")
		actionIterator->HasQualifier("Cross"))
	{
        return my::Null<int>();
    }

    int currentTeamId = actionIterator->m_teamId,
        currentPlayerId = actionIterator->m_playerId;

	// TESTING: (17-May-2018) IF IT WAS A SUCCESSFUL PASS, LET'S LOOK FOR THE NEXT EVENT INVOLVING THIS TEAM.

    ++actionIterator;

	while (actionIterator != actionArray.end())
	{
		switch (actionIterator->m_soccerEventType) {
		case my::sport::END_SOCCER_EVENT:
			return my::Null<int>();
			break;

		default:
			if ((actionIterator->m_teamId == currentTeamId) &&
				(actionIterator->m_playerId != currentPlayerId))
			{
				return actionIterator->m_playerId;
			}
		}

		++actionIterator;
	}

    return my::Null<int>();
}
