import React from 'react';
import { connect } from 'react-redux';
import { Tracing } from '../../components/tracing';
import {
  getFunctionsCalls,
  handleLimitChange,
  handleThresholdChange,
  toggleCallsTracing,
  toggleExpandItem,
  sortCallsBy,
  expandTracingPanel,
  shrinkTracingPanel,
} from '../../actions';
import {
  getFunctionCalls,
  getFunctionControl,
  getFunctionTracingVisibility,
  isConnection,
} from '../../selectors';

const TracingContainer = props => <Tracing {...props} />;

const mapStateToProps = (state, ownProps) => ({
  monitored: ownProps.monitored,
  calls: getFunctionCalls(state, ownProps.monitored.query),
  controls: getFunctionControl(state, ownProps.monitored.query),
  panelVisibility: getFunctionTracingVisibility(
    state,
    ownProps.monitored.query,
  ),
  isConnection: isConnection(state),
});

const mapDispatchToProps = {
  getFunctionsCalls,
  toggleCallsTracing,
  toggleExpandItem,
  handleThresholdChange,
  handleLimitChange,
  sortCallsBy,
  expandTracingPanel,
  shrinkTracingPanel,
};

const con = connect(mapStateToProps, mapDispatchToProps)(TracingContainer);
export default con;